# Default number of genes returned when the caller does not request a subset,
# and the maximum accepted when it does.
VST_DEFAULT_GENE_LIMIT <- 1000L

read_vst_matrix <- function(results_dir, key = NULL) {
  vst_path <- file.path(results_dir, "vst_mat.fst")
  if (!file.exists(vst_path)) {
    raise(error_not_found(
      "No expression data found",
      "The VST expression matrix is not available for this analysis."
    ))
  }

  vst_df <- read_session_fst(vst_path, key)
  gene_ids <- vst_df$gene_id
  vst_mat <- as.matrix(vst_df[, setdiff(names(vst_df), "gene_id"), drop = FALSE])
  rownames(vst_mat) <- gene_ids
  vst_mat
}

# `genes_param` is the comma-separated ?genes= value (empty means "first N").
# Unknown genes are ignored
filter_vst_genes <- function(vst_mat, genes_param = "",
                             max_genes = VST_DEFAULT_GENE_LIMIT) {
  genes_param <- as.character(genes_param)
  if (length(genes_param) == 0L || is.na(genes_param[1])) genes_param <- ""
  genes_param <- genes_param[1]

  if (nzchar(genes_param)) {
    genes <- strsplit(genes_param, ",", fixed = TRUE)[[1]]
    if (length(genes) > max_genes) genes <- genes[seq_len(max_genes)]
    matching_genes <- intersect(genes, rownames(vst_mat))
    if (length(matching_genes) == 0L) {
      raise(error_not_found(
        "No matching genes found",
        "None of the requested genes were found in the expression data."
      ))
    }
    return(vst_mat[matching_genes, , drop = FALSE])
  }

  n <- min(nrow(vst_mat), max_genes)
  vst_mat[seq_len(n), , drop = FALSE]
}

# Label samples as Group1 (reference) / Group2 (comparison) / Unknown using the
# analysis config
vst_sample_conditions <- function(vst_mat, config_file_path) {
  tryCatch(
    {
      cfg <- suppressWarnings(jsonlite::read_json(config_file_path))
      de_params <- cfg$de_parameters
      ref <- unlist(de_params$reference_samples)
      comp <- unlist(de_params$comparison_samples)
      if (is.null(ref)) ref <- character(0)
      if (is.null(comp)) comp <- character(0)
      vapply(colnames(vst_mat), function(s) {
        if (s %in% ref) {
          return("Group1")
        }
        if (s %in% comp) {
          return("Group2")
        }
        "Unknown"
      }, FUN.VALUE = character(1), USE.NAMES = FALSE)
    },
    error = function(e) {
      rep("Unknown", ncol(vst_mat))
    }
  )
}

vst_response_payload <- function(analysis_id, results_dir, key = NULL,
                                 genes_param = "",
                                 max_genes = VST_DEFAULT_GENE_LIMIT) {
  vst_mat <- read_vst_matrix(results_dir, key)
  vst_mat <- filter_vst_genes(vst_mat, genes_param, max_genes)

  list(
    gene_ids = rownames(vst_mat),
    samples = colnames(vst_mat),
    conditions = vst_sample_conditions(vst_mat, get_config_file_path(analysis_id)),
    matrix = lapply(seq_len(nrow(vst_mat)), function(i) as.numeric(vst_mat[i, ]))
  )
}
