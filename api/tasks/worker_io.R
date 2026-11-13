# DE writes de_results.fst / vst_mat.fst, and GSEA / ORA read them back through the
# load_de_* helpers. `key` is the browser-held encryption key (NULL for demos).

save_de_results <- function(analysis_id, de_result, key = NULL) {
  current_analysis_dir <- file.path(SESSION_DATA_DIR, analysis_id)

  if (!is.null(de_result$sigs)) {
    sigs_path <- file.path(current_analysis_dir, "de_results.fst")
    write_session_fst(de_result$sigs, sigs_path, key)
  }
  if (!is.null(de_result$norm_counts)) {
    norm_counts_path <- file.path(current_analysis_dir, "norm_counts.fst")
    write_session_fst(de_result$norm_counts, norm_counts_path, key)
  }
  if (!is.null(de_result$vst_mat)) {
    vst_path <- file.path(current_analysis_dir, "vst_mat.fst")
    write_session_fst(de_result$vst_mat, vst_path, key)
  }
  if (!is.null(de_result$dispersion_data)) {
    dispersion_path <- file.path(current_analysis_dir, "dispersion_data.fst")
    write_session_fst(de_result$dispersion_data, dispersion_path, key)
  }
  if (!is.null(de_result$de_summary)) {
    ds <- de_result$de_summary
    for (f in c("covariates", "dropped_covariates", "original_covariates", "collinear_dropped")) {
      if (!is.null(ds[[f]])) ds[[f]] <- I(as.character(unlist(ds[[f]])))
    }
    de_summary_path <- file.path(current_analysis_dir, "de_summary.json")
    jsonlite::write_json(ds, de_summary_path, auto_unbox = TRUE)
  }
}

save_gsea_results <- function(analysis_id, gsea_result, key = NULL) {
  gsea_path <- file.path(SESSION_DATA_DIR, analysis_id, "gsea_results.fst")
  write_session_fst(gsea_result, gsea_path, key)
}

save_gsea_rankings <- function(analysis_id, gene_list, key = NULL) {
  rankings <- data.frame(
    gene_id = names(gene_list),
    score = as.numeric(gene_list),
    stringsAsFactors = FALSE
  )
  rankings_path <- file.path(SESSION_DATA_DIR, analysis_id, "gsea_rankings.fst")
  write_session_fst(rankings, rankings_path, key)
}

save_explore_results <- function(analysis_id, explore_result, transformed_df, key = NULL) {
  current_analysis_dir <- file.path(SESSION_DATA_DIR, analysis_id)

  results_path <- file.path(current_analysis_dir, "explore_results.fst")
  write_session_fst(transformed_df, results_path, key)

  norm_counts <- explore_result$norm_counts
  norm_df <- data.frame(
    gene_id = rownames(norm_counts),
    as.data.frame(norm_counts),
    check.names = FALSE,
    row.names = NULL
  )
  norm_path <- file.path(current_analysis_dir, "explore_norm_counts.fst")
  write_session_fst(norm_df, norm_path, key)

  summary_path <- file.path(current_analysis_dir, "explore_summary.json")
  # NULL encodes as {} in jsonlite; NA encodes as null
  if (is.null(explore_result$filter_info$warning)) {
    explore_result$filter_info$warning <- NA_character_
  }
  jsonlite::write_json(explore_result$filter_info, summary_path, auto_unbox = TRUE, na = "null")
}

load_count_matrix <- function(counts_file_path, config, key = NULL) {
  # Note: config must be valid at this point since we validate it before starting the task
  name_col <- config$name_column

  raw_samples <- c(config$de_parameters$reference_samples, config$de_parameters$comparison_samples)
  selected_samples <- as.character(unlist(raw_samples))
  selected_samples <- selected_samples[!is.na(selected_samples) & selected_samples != ""]

  counts_dt <- read_session_fst(counts_file_path, key, as.data.table = TRUE)

  cols_to_select <- as.character(unlist(c(name_col, selected_samples)))
  sub_dt <- counts_dt[, cols_to_select, with = FALSE]

  # Preserve fractional raw counts for Limma; DESeq2 rounds at calculation time.
  sub_dt[, (selected_samples) := lapply(.SD, as.numeric), .SDcols = selected_samples]

  mat <- as.matrix(sub_dt[, ..selected_samples])
  storage.mode(mat) <- "double"
  rownames(mat) <- sub_dt[[name_col]]

  mat
}

load_explore_count_matrix <- function(counts_file_path, name_column, sample_columns, key = NULL) {

  counts_dt <- read_session_fst(counts_file_path, key, as.data.table = TRUE)

  missing_cols <- setdiff(c(name_column, sample_columns), names(counts_dt))
  if (length(missing_cols) > 0) {
    stop("Count data does not contain columns: ", paste(missing_cols, collapse = ", "))
  }

  sub_dt <- counts_dt[, c(name_column, sample_columns), with = FALSE]

  sub_dt[, (sample_columns) := lapply(.SD, as.numeric), .SDcols = sample_columns]

  mat <- as.matrix(sub_dt[, ..sample_columns])
  storage.mode(mat) <- "double"
  rownames(mat) <- sub_dt[[name_column]]

  mat
}

load_de_results_df <- function(analysis_id, metadata = NULL, key = NULL) {
  if (is.null(metadata)) {
    metadata_file <- file.path(SESSION_DATA_DIR, analysis_id, METADATA_FILE_NAME)
    metadata <- if (file.exists(metadata_file)) {
      jsonlite::read_json(metadata_file)
    } else {
      list(analysis_id = analysis_id)
    }
  }
  current_analysis_dir <- analysis_results_dir(metadata, "de")
  de_results_df <- NULL
  de_results_path <- file.path(current_analysis_dir, "de_results.fst")
  if (file.exists(de_results_path)) de_results_df <- read_session_fst(de_results_path, key)
  rownames(de_results_df) <- de_results_df$gene_id
  de_results_df
}

load_de_results_vst_mat <- function(analysis_id, metadata = NULL, key = NULL) {
  if (is.null(metadata)) {
    metadata_file <- file.path(SESSION_DATA_DIR, analysis_id, METADATA_FILE_NAME)
    metadata <- if (file.exists(metadata_file)) {
      jsonlite::read_json(metadata_file)
    } else {
      list(analysis_id = analysis_id)
    }
  }
  current_analysis_dir <- analysis_results_dir(metadata, "de")
  de_results_vst_mat <- NULL
  de_results_vst_path <- file.path(current_analysis_dir, "vst_mat.fst")
  if (file.exists(de_results_vst_path)) {
    vst_df <- read_session_fst(de_results_vst_path, key)
    # Convert data.frame with gene_id column back to matrix with rownames
    gene_id_col <- vst_df$gene_id
    de_results_vst_mat <- as.matrix(vst_df[, setdiff(names(vst_df), "gene_id"), drop = FALSE])
    rownames(de_results_vst_mat) <- gene_id_col
  }
  de_results_vst_mat
}
