create_analysis <- function() {
  analysis_id <- uuid::UUIDgenerate(n = 1, use.time = TRUE)
  analysis_dir <- file.path(SESSION_DATA_DIR, analysis_id)
  dir.create(analysis_dir, recursive = TRUE)

  metadata <- list(
    analysis_id = analysis_id,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
    status = "created",
    running = FALSE,
    configured = FALSE,
    de_complete = FALSE,
    gsea_complete = FALSE,
    ora_complete = FALSE,
    correlation_ora_complete = FALSE,
    explore_complete = FALSE,
    sample_count = 0,
    gene_count = 0
  )
  metadata_file <- file.path(analysis_dir, "metadata.json")
  tryCatch({
    write_json(metadata, metadata_file)
  }, error = function(e) {
    message("Failed to write analysis metadata")
    raise(error_internal_error("Internal server error", "Failed to save analysis metadata"))
  })
  metadata
}

get_analysis <- function(analysis_id) {
  metadata_file <- file.path(SESSION_DATA_DIR, analysis_id, "metadata.json")
  if (!file.exists(metadata_file)) raise(error_not_found("Analysis not found"))

  tryCatch({
    read_json(metadata_file)
  }, error = function(e) {
    message("Failed to read analysis metadata")
    raise(error_internal_error("Internal server error", "Failed to read analysis metadata"))
  })
}

update_analysis <- function(analysis, updates) {
  analysis_id <- analysis$analysis_id
  for (field in names(updates)) {
    analysis[[field]] <- updates[[field]]
  }
  metadata_file_path <- file.path(SESSION_DATA_DIR, analysis_id, "metadata.json")

  tryCatch({
    write_json(analysis, metadata_file_path, pretty = TRUE, auto_unbox = TRUE)
  }, error = function(e) {
    message("Failed to write analysis metadata")
    raise(error_internal_error("Internal server error", "Failed to save analysis metadata"))
  })

  analysis
}

is_configured <- function(analysis_id) {
  metadata <- get_analysis(analysis_id)
  !is.null(metadata) && isTRUE(metadata$configured)
}
