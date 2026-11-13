# Content-hash results cache (demo only) --------------------------------------
# Only demo analyses use this shared cache. Private analyses are encrypted
# with a browser-held key and always read/write their own session directory:
# the run routes never compute a cache key for them, and the worker only
# mirrors results when the task carries an explicit use_cache = TRUE flag.
#
# Deterministic demo results (DE, explore) are stored under a shared, read-only
# directory keyed by sha1(content hash + settings + cache version), so an
# identical re-run completes instantly without recomputation. Demo data is
# public, so cache entries are plain files (no per-analysis encryption).
#
# Storage is plain fst/json files in api/cache/{key}/: the
# result routes stream fst files in row chunks directly from disk, so a cache
# entry must *be* those files.
#
# The demo session's metadata.json records which key it used (de_cache_key /
# explore_cache_key). Result-serving code resolves the base directory with
# analysis_results_dir(); there is no copying back into session dirs.
# Evicted-but-referenced keys resolve to the session dir, where the files no
# longer exist — the serving route reports "results expired, re-run".
#
# TTL matches the privacy policy. purge_expired_results_cache() and
# purge_expired_sessions() run hourly from the worker monitor loop.

CACHE_DIR <- Sys.getenv("GENLYZER_CACHE_DIR", "api/cache")
CACHE_TTL_DAYS <- as.numeric(Sys.getenv("CACHE_TTL_DAYS", "7"))
CACHE_VERSION <- "v1"

DE_RESULT_FILES <- c(
  "de_results.fst", "norm_counts.fst", "vst_mat.fst",
  "dispersion_data.fst", "de_summary.json"
)
EXPLORE_RESULT_FILES <- c(
  "explore_results.fst", "explore_norm_counts.fst", "explore_summary.json"
)

hash_file <- function(path) {
  digest::digest(file = path, algo = "sha1")
}

hash_key_material <- function(material) {
  json <- jsonlite::toJSON(
    material, auto_unbox = TRUE, null = "null", na = "null", digits = NA
  )
  digest::digest(json, algo = "sha1")
}

de_cache_key <- function(counts_hash, sample_sheet_hash, config) {
  hash_key_material(list(
    version = CACHE_VERSION,
    kind = "de",
    counts = counts_hash,
    sample_sheet = sample_sheet_hash %||% "",
    organism = config$organism %||% "",
    de_parameters = config$de_parameters
  ))
}

# params is the normalized output of validate_explore_parameters().
explore_cache_key <- function(counts_hash, params) {
  hash_key_material(list(
    version = CACHE_VERSION,
    kind = "explore",
    counts = counts_hash,
    organism = params$organism %||% "",
    name_column = params$name_column %||% "",
    sample_columns = as.character(unlist(params$sample_columns)),
    explore_parameters = params$explore_parameters
  ))
}

cache_dir_for <- function(key) {
  file.path(CACHE_DIR, key)
}

cache_has_results <- function(key) {
  !is.null(key) && length(key) == 1L && is.character(key) &&
    nzchar(key) && dir.exists(cache_dir_for(key))
}

# Mirror result files into the shared cache atomically. Safe to call
# concurrently: the first writer's rename wins, later calls are no-ops.
cache_store_files <- function(key, src_dir, files) {
  if (is.null(key) || length(key) != 1L || !is.character(key) || !nzchar(key)) {
    return(invisible(FALSE))
  }
  final_dir <- cache_dir_for(key)
  if (dir.exists(final_dir)) {
    return(invisible(TRUE))
  }

  dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
  tmp_dir <- file.path(CACHE_DIR, paste0(".tmp-", key, "-", Sys.getpid()))
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
  dir.create(tmp_dir, recursive = TRUE)

  stored <- 0L
  for (f in files) {
    src <- file.path(src_dir, f)
    if (file.exists(src)) {
      ok <- file.copy(src, file.path(tmp_dir, f), overwrite = TRUE, copy.mode = TRUE)
      if (!ok) {
        message("Failed to stage demo cache result")
        return(invisible(FALSE))
      }
      stored <- stored + 1L
    }
  }
  if (stored == 0L) {
    return(invisible(FALSE))
  }

  if (!file.rename(tmp_dir, final_dir)) {
    # Lost the race against a concurrent store: the other copy is complete.
    if (dir.exists(final_dir)) {
      return(invisible(TRUE))
    }
    message("Failed to store demo cache results")
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

analysis_results_dir <- function(analysis, kind = c("de", "explore")) {
  kind <- match.arg(kind)
  # Only demo analyses ever record a cache key. Private analyses fall through
  # to their own encrypted session directory.
  key <- analysis[[paste0(kind, "_cache_key")]]
  if (!is.null(key) && length(key) == 1L && is.character(key) && nzchar(key)) {
    cached <- cache_dir_for(key)
    if (dir.exists(cached)) return(cached)
  }
  file.path(SESSION_DATA_DIR, analysis$analysis_id)
}

parse_use_cache_flag <- function(body) {
  if (is.list(body) && !is.null(body$use_cache)) {
    val <- body$use_cache
    if (is.logical(val)) return(isTRUE(val))
    if (is.character(val)) return(tolower(val[[1]]) %in% c("true", "1"))
  }
  TRUE
}

# --- Janitor (hourly, worker monitor process) -------------------------------

purge_expired_results_cache <- function(now = Sys.time()) {
  if (!dir.exists(CACHE_DIR)) return(invisible(0L))
  entries <- list.dirs(CACHE_DIR, recursive = FALSE, full.names = TRUE)
  # Skip staging dirs (.tmp-*): an in-flight store owns them.
  entries <- entries[!startsWith(basename(entries), ".")]
  purged <- 0L
  for (dir in entries) {
    mtime <- file.mtime(dir)
    age_days <- as.numeric(difftime(now, mtime, units = "days"))
    if (is.finite(age_days) && age_days > CACHE_TTL_DAYS) {
      unlink(dir, recursive = TRUE, force = TRUE)
      if (!dir.exists(dir)) {
        purged <- purged + 1L
      }
    }
  }
  invisible(purged)
}

# Retention mirrors the privacy policy's session-data promise. Sessions still
# marked running are never deleted. Demo sessions are ordinary sessions seeded
# from public data, so they age out like any other.
purge_expired_sessions <- function(now = Sys.time(), max_age_days = NULL) {
  if (is.null(max_age_days)) {
    max_age_days <- if (exists("CLEANUP_AGE_DAYS")) {
      CLEANUP_AGE_DAYS
    } else {
      as.numeric(Sys.getenv("CLEANUP_AGE_DAYS", "1"))
    }
  }
  if (!dir.exists(SESSION_DATA_DIR)) return(invisible(0L))
  entries <- list.dirs(SESSION_DATA_DIR, recursive = FALSE, full.names = TRUE)
  purged <- 0L
  for (dir in entries) {
    id <- basename(dir)
    metadata_path <- file.path(dir, "metadata.json")
    if (file.exists(metadata_path)) {
      metadata <- tryCatch(jsonlite::read_json(metadata_path), error = function(e) NULL)
      # unlist() tolerates both unboxed scalars and length-1 array shapes.
      running <- !is.null(metadata$running) && isTRUE(as.logical(unlist(metadata$running)))
      if (running) next
      mtime <- file.mtime(metadata_path)
    } else {
      mtime <- file.mtime(dir)
    }
    age_days <- as.numeric(difftime(now, mtime, units = "days"))
    if (is.finite(age_days) && age_days > max_age_days) {
      unlink(dir, recursive = TRUE, force = TRUE)
      if (!dir.exists(dir)) {
        purged <- purged + 1L
      }
    }
  }
  invisible(purged)
}
