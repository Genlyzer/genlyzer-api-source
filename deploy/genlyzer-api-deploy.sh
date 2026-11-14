#!/usr/bin/env bash
set -Eeuo pipefail
umask 077

APP_DIR=/opt/genlyzer
SERVICE_USER=genlyzer
INCOMING_DIR=/var/lib/genlyzer-deploy/incoming
STATE_DIR=/var/lib/genlyzer-api-deploy
BACKUP_ROOT="$STATE_DIR/backups"
BUILD_INFO=/etc/genlyzer/build-info

api_stopped=0
worker_stopped=0
changes_applied=0
work_dir=
backup_dir=
build_info_tmp=

fail() {
  printf 'ERROR: %s\n' "$*" >&2
  if declare -F recover_on_failure >/dev/null 2>&1; then
    recover_on_failure
  fi
  exit 1
}

if (( EUID != 0 )); then
  fail "Run this script through the restricted sudo rule."
fi
(( $# == 0 )) || fail "This command does not accept arguments."

for command in curl flock install python3 rsync sha256sum sudo systemctl tar; do
  command -v "$command" >/dev/null 2>&1 || fail "Required command is missing: $command"
done

[[ -d "$APP_DIR/api" && -f "$APP_DIR/openapi.yaml" ]] ||
  fail "Expected existing API installation under $APP_DIR."
[[ -d "$INCOMING_DIR" && ! -L "$INCOMING_DIR" ]] ||
  fail "Deployment incoming directory is missing or is a symlink."
[[ "$(stat -c '%U:%a' "$INCOMING_DIR")" == "genlyzer-deploy:700" ]] ||
  fail "Deployment incoming directory must be owned by genlyzer-deploy with mode 700."
[[ -x /usr/bin/Rscript ]] || fail "Rscript is missing."
id "$SERVICE_USER" >/dev/null 2>&1 || fail "Service account '$SERVICE_USER' is missing."

install -d -o root -g root -m 0700 "$BACKUP_ROOT"
install -d -o root -g root -m 0755 /etc/genlyzer
exec 9>/run/lock/genlyzer-api-deploy.lock
flock -n 9 || fail "Another API deployment is already running."

work_dir="$(mktemp -d "$STATE_DIR/work.XXXXXXXX")"
chmod 0700 "$work_dir"
trap 'cleanup' EXIT

cleanup() {
  if [[ -n "$build_info_tmp" && -e "$build_info_tmp" ]]; then
    rm -f -- "$build_info_tmp"
  fi
  if [[ -n "$work_dir" && "$work_dir" == "$STATE_DIR"/work.* && -d "$work_dir" ]]; then
    rm -rf -- "$work_dir"
  fi
}

rollback() {
  local failed=0
  set +e
  systemctl stop genlyzer-api >/dev/null 2>&1 || failed=1
  systemctl stop genlyzer-worker >/dev/null 2>&1 || failed=1
  rsync -a --delete \
    --exclude='/session_data/' \
    --exclude='/queue/' \
    --exclude='/queue_liteq/' \
    --exclude='/cache/' \
    --exclude='/logs/' \
    --exclude='/server.out' \
    "$backup_dir/api/" "$APP_DIR/api/" || failed=1
  install -o root -g root -m 0644 "$backup_dir/openapi.yaml" "$APP_DIR/openapi.yaml" || failed=1
  if [[ -f "$backup_dir/build-info" ]]; then
    install -o root -g root -m 0644 "$backup_dir/build-info" "$BUILD_INFO" || failed=1
  else
    rm -f -- "$BUILD_INFO" || failed=1
  fi
  systemctl start genlyzer-worker || failed=1
  systemctl start genlyzer-api || failed=1
  set -e
  return "$failed"
}

recover_on_failure() {
  if [[ "$changes_applied" == 1 ]]; then
    printf 'Deployment failed; restoring the previous API code.\n' >&2
    if ! rollback; then
      printf 'ERROR: Automatic rollback was incomplete; inspect both systemd units.\n' >&2
    else
      changes_applied=0
    fi
  else
    if [[ "$worker_stopped" == 1 ]]; then
      systemctl start genlyzer-worker || printf 'ERROR: Could not restart genlyzer-worker.\n' >&2
      worker_stopped=0
    fi
    if [[ "$api_stopped" == 1 ]]; then
      systemctl start genlyzer-api || printf 'ERROR: Could not restart genlyzer-api.\n' >&2
      api_stopped=0
    fi
  fi
}

on_error() {
  local status=$?
  trap - ERR
  recover_on_failure
  exit "$status"
}
trap 'on_error' ERR

copy_incoming_files() {
  python3 - "$INCOMING_DIR" "$work_dir" <<'PY'
import os
import shutil
import stat
import sys

source_dir, target_dir = sys.argv[1:]
dir_fd = os.open(source_dir, os.O_RDONLY | os.O_DIRECTORY)
try:
    for name, max_size in (
        ("api-source.tar.gz", 512 * 1024 * 1024),
        ("build-info", 4096),
    ):
        fd = os.open(name, os.O_RDONLY | os.O_NOFOLLOW, dir_fd=dir_fd)
        try:
            info = os.fstat(fd)
            if not stat.S_ISREG(info.st_mode) or info.st_size > max_size:
                raise SystemExit(f"Invalid incoming file: {name}")
            with os.fdopen(fd, "rb", closefd=False) as src:
                with open(os.path.join(target_dir, name), "xb") as dst:
                    shutil.copyfileobj(src, dst)
        finally:
            os.close(fd)
finally:
    os.close(dir_fd)
PY
}

validate_archive() {
  python3 - "$work_dir/api-source.tar.gz" <<'PY'
import sys
import tarfile
from pathlib import PurePosixPath

archive_path = sys.argv[1]
prefix = "genlyzer-api"
allowed_dirs = {
    prefix,
    f"{prefix}/api",
    f"{prefix}/api/config",
    f"{prefix}/api/helpers",
    f"{prefix}/api/routes",
    f"{prefix}/api/tasks",
    f"{prefix}/api/demo-data",
}
allowed_files = {
    f"{prefix}/openapi.yaml",
    f"{prefix}/api/server.R",
    f"{prefix}/api/config/database_config.R",
    f"{prefix}/api/demo-data/demo_counts.xls",
    f"{prefix}/api/demo-data/demo_sample_sheet.csv",
}
required_files = allowed_files | {
    f"{prefix}/api/helpers/build_info.R",
    f"{prefix}/api/routes/analyses.R",
    f"{prefix}/api/tasks/start_workers.R",
}
max_uncompressed_size = 512 * 1024 * 1024

try:
    with tarfile.open(archive_path, "r:gz") as archive:
        members = archive.getmembers()
        names = set()
        total_size = 0
        for member in members:
            path = PurePosixPath(member.name.rstrip("/"))
            name = path.as_posix()
            if path.is_absolute() or ".." in path.parts or not path.parts or path.parts[0] != prefix:
                raise ValueError(f"Unsafe archive path: {member.name}")
            if member.isdir():
                if name not in allowed_dirs:
                    raise ValueError(f"Unexpected directory: {member.name}")
            elif member.isreg():
                if name in names:
                    raise ValueError(f"Duplicate archive entry: {member.name}")
                parts = path.parts
                is_r_source = (
                    len(parts) == 4
                    and parts[2] in {"helpers", "routes", "tasks"}
                    and parts[0:2] == (prefix, "api")
                    and parts[3].endswith(".R")
                )
                if name not in allowed_files and not is_r_source:
                    raise ValueError(f"Unexpected file: {member.name}")
                names.add(name)
                total_size += member.size
                if total_size > max_uncompressed_size:
                    raise ValueError("Uncompressed artifact is too large")
            else:
                raise ValueError(f"Unsupported archive entry: {member.name}")
        missing = sorted(required_files - names)
        if missing:
            raise ValueError("Required API files are missing: " + ", ".join(missing))
except (OSError, tarfile.TarError, ValueError) as error:
    print(f"Invalid API artifact: {error}", file=sys.stderr)
    raise SystemExit(1)
PY
}

copy_incoming_files
mapfile -t metadata_lines < "$work_dir/build-info"
[[ "${#metadata_lines[@]}" == 4 ]] || fail "Build metadata must contain exactly four lines."
[[ "${metadata_lines[0]}" == source_revision=* ]] || fail "Invalid source revision metadata."
[[ "${metadata_lines[1]}" == artifact_digest=* ]] || fail "Invalid artifact digest metadata."
[[ "${metadata_lines[2]}" == provenance_url=* ]] || fail "Invalid provenance metadata."
[[ "${metadata_lines[3]}" == built_at=* ]] || fail "Invalid build timestamp metadata."
source_revision="${metadata_lines[0]#source_revision=}"
artifact_digest="${metadata_lines[1]#artifact_digest=}"
provenance_url="${metadata_lines[2]#provenance_url=}"
built_at="${metadata_lines[3]#built_at=}"
[[ "$source_revision" =~ ^([[:xdigit:]]{40}|[[:xdigit:]]{64})$ ]] ||
  fail "Source revision is not a full Git hash."
[[ "$artifact_digest" =~ ^sha256:[[:xdigit:]]{64}$ ]] ||
  fail "Artifact digest is not a SHA-256 digest."
[[ "$provenance_url" =~ ^https://github\.com/[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+/actions/runs/[0-9]+$ ]] ||
  fail "Provenance URL is not a GitHub Actions run URL."
[[ "$built_at" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}T[^[:space:]]+Z$ ]] ||
  fail "Build timestamp is not in UTC ISO 8601 format."

actual_digest="sha256:$(sha256sum "$work_dir/api-source.tar.gz" | cut -d ' ' -f 1)"
[[ "$actual_digest" == "$artifact_digest" ]] || fail "Artifact digest does not match the metadata."
validate_archive
mkdir "$work_dir/extracted"
tar -xzf "$work_dir/api-source.tar.gz" --no-same-owner -C "$work_dir/extracted"
[[ -d "$work_dir/extracted/genlyzer-api/api" ]] || fail "API artifact did not extract correctly."

systemctl is-active --quiet genlyzer-api || fail "genlyzer-api must be active before deployment."
systemctl is-active --quiet genlyzer-worker || fail "genlyzer-worker must be active before deployment."
systemctl stop genlyzer-api
api_stopped=1

if ! queue_counts="$(
  cd "$APP_DIR"
  sudo -H -u "$SERVICE_USER" /usr/bin/Rscript --vanilla - <<'RS'
queue_path <- "api/queue_liteq/liteq.sqlite"
if (!file.exists(queue_path)) stop("Queue database is unavailable")
source("api/helpers/queue_stats.R")
check_queue_table <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (!DBI::dbExistsTable(con, queue_table_name())) {
    stop("Queue table is unavailable")
  }
}
check_queue_table(queue_path)
counts <- queue_status_counts(queue_path)
cat(counts$ready, counts$working, sep = " ")
RS
)"; then
  fail "Could not safely inspect the analysis queue; no code was changed."
fi
read -r queued_count working_count <<< "$queue_counts"
[[ "$queued_count" =~ ^[0-9]+$ && "$working_count" =~ ^[0-9]+$ ]] ||
  fail "Queue status could not be parsed; no code was changed."
if (( queued_count > 0 || working_count > 0 )); then
  systemctl start genlyzer-api
  api_stopped=0
  fail "Deployment blocked: queue has $queued_count queued and $working_count running analysis task(s)."
fi

systemctl stop genlyzer-worker
worker_stopped=1
backup_dir="$(mktemp -d "$BACKUP_ROOT/api-before.XXXXXXXX")"
rsync -a \
  --exclude='/session_data/' \
  --exclude='/queue/' \
  --exclude='/queue_liteq/' \
  --exclude='/cache/' \
  --exclude='/logs/' \
  --exclude='/server.out' \
  "$APP_DIR/api/" "$backup_dir/api/"
cp -p "$APP_DIR/openapi.yaml" "$backup_dir/openapi.yaml"
if [[ -f "$BUILD_INFO" ]]; then
  cp -p "$BUILD_INFO" "$backup_dir/build-info"
fi

changes_applied=1
rsync -a --delete \
  --exclude='/session_data/' \
  --exclude='/queue/' \
  --exclude='/queue_liteq/' \
  --exclude='/cache/' \
  --exclude='/logs/' \
  --exclude='/server.out' \
  "$work_dir/extracted/genlyzer-api/api/" "$APP_DIR/api/"
install -o root -g root -m 0644 \
  "$work_dir/extracted/genlyzer-api/openapi.yaml" "$APP_DIR/openapi.yaml"

build_info_tmp="$(mktemp /etc/genlyzer/build-info.XXXXXXXX)"
printf 'GENLYZER_SOURCE_REVISION=%s\n' "$source_revision" > "$build_info_tmp"
printf 'GENLYZER_ARTIFACT_DIGEST=%s\n' "$artifact_digest" >> "$build_info_tmp"
printf 'GENLYZER_BUILD_PROVENANCE_URL=%s\n' "$provenance_url" >> "$build_info_tmp"
printf 'GENLYZER_BUILD_TIMESTAMP=%s\n' "$built_at" >> "$build_info_tmp"
chown root:root "$build_info_tmp"
chmod 0644 "$build_info_tmp"
mv -f -- "$build_info_tmp" "$BUILD_INFO"
build_info_tmp=

systemctl start genlyzer-worker
worker_stopped=0
systemctl start genlyzer-api
api_stopped=0

healthy=0
for attempt in $(seq 1 30); do
  if build_response="$(curl -fsS --max-time 3 http://127.0.0.1:8080/v1/build-info 2>/dev/null)"; then
    if printf '%s' "$build_response" | python3 -c '
import json, sys
expected_revision, expected_digest, expected_url = sys.argv[1:]
data = json.load(sys.stdin)
sys.exit(0 if (
    data.get("metadata_status") == "complete"
    and data.get("source_revision") == expected_revision
    and data.get("artifact_digest") == expected_digest
    and data.get("provenance_url") == expected_url
) else 1)
' "$source_revision" "$artifact_digest" "$provenance_url"; then
      if versions_response="$(curl -fsS --max-time 3 http://127.0.0.1:8080/v1/versions 2>/dev/null)" &&
          printf '%s' "$versions_response" | python3 -c '
import json, sys
data = json.load(sys.stdin)
sys.exit(0 if isinstance(data, dict) and {"mus_musculus", "homo_sapiens"} <= data.keys() else 1)
'; then
        healthy=1
        break
      fi
    fi
  fi
  sleep 2
done
(( healthy == 1 )) || fail "Post-deployment health check failed."
systemctl is-active --quiet genlyzer-worker || fail "genlyzer-worker is not active after deployment."
systemctl is-active --quiet genlyzer-api || fail "genlyzer-api is not active after deployment."

changes_applied=0
printf 'Deployed API revision %s (%s).\n' "$source_revision" "$artifact_digest"
