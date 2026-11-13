# ORA request helpers ---------------------------------------------------------
# run-ora and run-correlation-ora share collection parsing (see
# parse_collection_value() in validation.R). Correlation ORA has extra request
# validation: organism-specific Ensembl IDs, a still-current explore universe,
# and the reduction of posted correlation results to the significant gene set.

validate_correlation_ora_request <- function(body) {
  body <- body %||% list()
  collection_value <- body$collection
  correlation_padj_threshold <- body$correlation_padj_threshold
  padj_threshold <- body$padj_threshold %||% 0.05
  correlation_gene_ids <- unlist(body$correlation_gene_ids, use.names = FALSE)
  correlation_padj_values <- unlist(body$correlation_padj_values, use.names = FALSE)

  if (is.null(collection_value) || !is.character(collection_value) ||
        length(collection_value) != 1L || !nzchar(collection_value)) {
    raise(error_bad_request("Invalid request", "Collection is required."))
  }
  if (!is.numeric(correlation_padj_threshold) ||
        length(correlation_padj_threshold) != 1L ||
        is.na(correlation_padj_threshold) || correlation_padj_threshold < 0 ||
        correlation_padj_threshold > 1) {
    raise(error_bad_request(
      "Invalid request",
      "correlation_padj_threshold must be a number in [0, 1]."
    ))
  }
  if (!is.numeric(padj_threshold) || length(padj_threshold) != 1L ||
        is.na(padj_threshold) || padj_threshold < 0 || padj_threshold > 1) {
    raise(error_bad_request(
      "Invalid request",
      "padj_threshold must be a number in [0, 1]."
    ))
  }
  if (!is.character(correlation_gene_ids) || length(correlation_gene_ids) == 0L ||
        anyNA(correlation_gene_ids) || any(!nzchar(correlation_gene_ids)) ||
        anyDuplicated(correlation_gene_ids)) {
    raise(error_bad_request(
      "Invalid request",
      "correlation_gene_ids must be a non-empty array of unique gene IDs."
    ))
  }
  if (!is.numeric(correlation_padj_values) ||
        length(correlation_padj_values) != length(correlation_gene_ids) ||
        any(!is.finite(correlation_padj_values)) ||
        any(correlation_padj_values < 0 | correlation_padj_values > 1)) {
    raise(error_bad_request(
      "Invalid request",
      "correlation_padj_values must contain one adjusted p-value in [0, 1] for each gene ID."
    ))
  }

  list(
    collection_value = collection_value,
    correlation_padj_threshold = correlation_padj_threshold,
    padj_threshold = padj_threshold,
    correlation_gene_ids = correlation_gene_ids,
    correlation_padj_values = correlation_padj_values
  )
}

correlation_id_pattern <- function(organism) {
  if (identical(organism, "Homo sapiens")) {
    return("^ENSG[0-9]+([.][0-9]+)?$")
  }
  if (identical(organism, "Mus musculus")) {
    return("^ENSMUSG[0-9]+([.][0-9]+)?$")
  }
  raise(error_unprocessable(
    "Unsupported organism",
    "Correlation ORA supports human and mouse analyses."
  ))
}

analysis_organism <- function(analysis, analysis_id) {
  organism <- analysis$organism
  if (!is.null(organism) && length(organism) == 1L &&
        nzchar(as.character(organism))) {
    return(as.character(organism))
  }
  config_path <- get_config_file_path(analysis_id)
  config <- if (file.exists(config_path)) {
    jsonlite::read_json(config_path)
  } else {
    list()
  }
  config$organism
}

read_explore_gene_ids <- function(analysis, analysis_id, session_key) {
  explore_results_path <- file.path(
    analysis_results_dir(analysis, "explore"), "explore_results.fst"
  )
  if (!file.exists(explore_results_path)) {
    raise(error_gone(
      "Explore results expired",
      "Please run exploration again before running correlation ORA."
    ))
  }
  as.character(
    read_session_fst(explore_results_path, session_key, columns = "gene_id")$gene_id
  )
}

# Validate the posted correlation results against the session, then return the
# queue payload for run-correlation-ora. `universe` is every gene the client
# tested; `gene_list` only the ones passing the correlation threshold.
prepare_correlation_ora <- function(body, analysis, analysis_id, session_key) {
  request <- validate_correlation_ora_request(body)

  id_pattern <- correlation_id_pattern(
    analysis_organism(analysis, analysis_id)
  )
  if (any(!grepl(id_pattern, request$correlation_gene_ids))) {
    raise(error_bad_request(
      "Invalid gene identifiers",
      "Correlation ORA requires Ensembl gene IDs matching the analysis organism."
    ))
  }

  explored_gene_ids <- read_explore_gene_ids(analysis, analysis_id, session_key)
  if (!all(request$correlation_gene_ids %in% explored_gene_ids)) {
    raise(error_bad_request(
      "Correlation results are stale",
      "Recalculate correlation using the current explore results."
    ))
  }

  gene_list <- request$correlation_gene_ids[
    request$correlation_padj_values <= request$correlation_padj_threshold
  ]
  if (length(gene_list) == 0L) {
    raise(error_unprocessable(
      "No significant correlation genes",
      "No genes meet the correlation adjusted p-value cutoff."
    ))
  }

  which_collection <- tryCatch(
    parse_collection_value(request$collection_value),
    error = function(e) raise(error_bad_request("Invalid request", conditionMessage(e)))
  )

  list(
    collection = which_collection$collection,
    value = which_collection$value,
    gene_list = gene_list,
    universe = request$correlation_gene_ids,
    padj_threshold = request$padj_threshold
  )
}
