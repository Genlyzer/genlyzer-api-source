build_info_value <- function(env, name) {
  if (is.null(names(env)) || !(name %in% names(env))) return(NULL)
  value <- env[[name]]
  if (is.null(value) || length(value) != 1L || is.na(value)) return(NULL)
  value <- trimws(as.character(value))
  if (!nzchar(value)) NULL else value
}

get_build_info <- function(env = Sys.getenv()) {
  source_revision <- build_info_value(env, "GENLYZER_SOURCE_REVISION")
  artifact_digest <- build_info_value(env, "GENLYZER_ARTIFACT_DIGEST")
  provenance_url <- build_info_value(env, "GENLYZER_BUILD_PROVENANCE_URL")
  built_at <- build_info_value(env, "GENLYZER_BUILD_TIMESTAMP")

  if (!is.null(source_revision) &&
        !grepl("^([0-9a-fA-F]{40}|[0-9a-fA-F]{64})$", source_revision)) {
    source_revision <- NULL
  }
  if (!is.null(artifact_digest) &&
        !grepl("^sha256:[0-9a-fA-F]{64}$", artifact_digest)) {
    artifact_digest <- NULL
  }
  if (!is.null(provenance_url) &&
        !grepl("^https://[^[:space:]]+$", provenance_url)) {
    provenance_url <- NULL
  }
  if (!is.null(built_at) &&
        !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[^[:space:]]+Z$", built_at)) {
    built_at <- NULL
  }

  complete <- !is.null(source_revision) &&
    !is.null(artifact_digest) &&
    !is.null(provenance_url)

  list(
    metadata_status = if (complete) "complete" else "incomplete",
    source_revision = source_revision,
    artifact_digest = artifact_digest,
    provenance_url = provenance_url,
    built_at = built_at,
    verification = "informational_only"
  )
}
