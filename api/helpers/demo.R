# A demo is just a normal analysis seeded with the bundled public dataset.
#
# Only differences are:
# 1) create_demo_analysis (called by POST /v1/analyses?demo=true) creates a
# fresh analysis for the caller and copies the demo counts + sample sheet into
# it, without the need to upload them separately to the analysis.
#
# 2) Demo sessions are public data, so their FST files (eg. for subsequent DE or Explore runs)
# are not encrypted with a browser-held key.

DEMO_COUNTS_FILE <- Sys.getenv("DEMO_COUNTS_FILE", "api/demo-data/demo_counts.xls")
DEMO_SHEET_FILE <- Sys.getenv("DEMO_SHEET_FILE", "api/demo-data/demo_sample_sheet.csv")

is_demo_analysis <- function(analysis_id) {
  if (is.null(analysis_id) || length(analysis_id) != 1L ||
        !is.character(analysis_id) || !nzchar(analysis_id)) {
    return(FALSE)
  }
  metadata <- tryCatch(get_analysis(analysis_id), error = function(e) NULL)
  isTRUE(metadata$demo)
}

parse_demo_flag <- function(value) {
  if (is.null(value) || length(value) == 0L) return(FALSE)
  if (is.logical(value)) return(isTRUE(value[[1]]))
  tolower(as.character(value[[1]])) %in% c("true", "1", "yes")
}

demo_config <- function(name_column, sample_columns) {
  list(
    organism = "Mus musculus",
    name_column = name_column,
    sample_columns = as.character(sample_columns),
    de_parameters = list(
      model = "DESeq2",
      gene_filter = list(type = "filter1", min_count_sum = 10),
      reference_samples = c("C1", "C2", "C3", "C4"),
      comparison_samples = c("LKO1", "LKO2", "LKO3", "LKO4"),
      alpha = 0.05
    )
  )
}

create_demo_analysis <- function() {
  if (!file.exists(DEMO_COUNTS_FILE) || !file.exists(DEMO_SHEET_FILE)) {
    raise(error_internal_error(
      "Demo data unavailable",
      "The bundled demo dataset could not be found on the server."
    ))
  }

  analysis <- create_analysis()
  analysis_id <- analysis$analysis_id

  tryCatch(
    {
      counts_df <- readxl::read_excel(DEMO_COUNTS_FILE)
      process_count_data(counts_df, analysis_id)

      sheet_df <- utils::read.csv(DEMO_SHEET_FILE, stringsAsFactors = FALSE)
      process_sample_sheet(sheet_df, analysis_id)

      name_column <- names(counts_df)[1]
      sample_columns <- setdiff(names(counts_df), name_column)
      config <- demo_config(name_column, sample_columns)
      valid_config <- validate_config(config, analysis_id)
      jsonlite::write_json(
        valid_config, get_config_file_path(analysis_id),
        pretty = TRUE, auto_unbox = TRUE
      )

      update_analysis(analysis, list(
        status = "configured",
        configured = TRUE,
        counts_uploaded = TRUE,
        de_complete = FALSE,
        explore_complete = FALSE,
        sample_count = length(sample_columns),
        gene_count = nrow(counts_df),
        demo = TRUE,
        counts_hash = hash_file(get_counts_file_path(analysis_id)),
        sample_sheet_hash = hash_file(get_sample_sheet_path(analysis_id)),
        message = "Demo dataset loaded"
      ))
    },
    error = function(e) {
      unlink(file.path(SESSION_DATA_DIR, analysis_id), recursive = TRUE, force = TRUE)
      stop(e)
    }
  )
}

# --- Demo-only results cache parameters --------------------------------------
# The shared results cache is an optimization for the public demo dataset.
# These helpers resolve the cache fields and, on a hit, the analysis metadata
# describing the already-finished run.
# Private analyses never get a cache key, so their results are always computed into
# their own encrypted session dir.
demo_de_cache_params <- function(analysis, body, config) {
  use_cache <- isTRUE(analysis$demo) && parse_use_cache_flag(body)
  cache_key <- NULL
  if (use_cache && !is.null(analysis$counts_hash) && nzchar(analysis$counts_hash)) {
    cache_key <- de_cache_key(
      analysis$counts_hash, analysis$sample_sheet_hash, config
    )
  }
  hit <- use_cache && !is.null(cache_key) && cache_has_results(cache_key)

  list(
    use_cache = use_cache,
    cache_key = cache_key,
    hit = hit,
    # Analysis metadata for the completed run. The route writes exactly these
    # fields to the stored analysis (update_analysis) and echoes them back, so
    # the analysis on disk and the response body stay identical.
    hit_analysis_meta = if (hit) {
      list(
        status = "de_complete",
        running = FALSE,
        de_complete = TRUE,
        task_id = "cache-hit",
        gsea_complete = FALSE,
        cached = TRUE,
        de_cache_key = cache_key,
        message = "DE analysis completed successfully"
      )
    } else {
      NULL
    }
  )
}

demo_explore_cache_params <- function(analysis, body, params) {
  use_cache <- isTRUE(analysis$demo) && parse_use_cache_flag(body)
  cache_key <- NULL
  if (use_cache && !is.null(analysis$counts_hash) && nzchar(analysis$counts_hash)) {
    cache_key <- explore_cache_key(analysis$counts_hash, params)
  }
  hit <- use_cache && !is.null(cache_key) && cache_has_results(cache_key)

  list(
    use_cache = use_cache,
    cache_key = cache_key,
    hit = hit,
    hit_analysis_meta = if (hit) {
      list(
        status = "explore_complete",
        running = FALSE,
        task_id = "cache-hit",
        explore_complete = TRUE,
        organism = params$organism,
        cached = TRUE,
        explore_cache_key = cache_key,
        message = "Exploration completed successfully"
      )
    } else {
      NULL
    }
  )
}
