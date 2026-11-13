# Explore (data transformation) task
# the monitor process supplies the count matrix and validated parameters

#' @param count_mat Numeric matrix of raw counts (genes x samples), rownames are gene IDs
#' @param explore_params List: transformation, normalization, pseudocount,
#'   gene_filter (type / min_count_sum / min_count_per_sample / min_samples),
#'   variance_filter (mode percent|count, value)
perform_explore_data_transformation <- function(count_mat, explore_params) {
  transformation <- explore_params$transformation
  normalization <- explore_params$normalization
  pseudocount <- explore_params$pseudocount
  gene_filter <- explore_params$gene_filter
  variance_filter <- explore_params$variance_filter

  # Preserve fractional raw counts
  row_names <- rownames(count_mat)
  storage.mode(count_mat) <- "double"

  total_genes_before <- nrow(count_mat)
  filter_type <- gene_filter$type %||% "filter1"
  if (filter_type == "filter1") {
    keep <- rowSums(count_mat) >= as.numeric(gene_filter$min_count_sum)
  } else if (filter_type == "filter2") {
    keep <- rowSums(count_mat >= as.numeric(gene_filter$min_count_per_sample)) >= as.numeric(gene_filter$min_samples)
  } else {
    keep <- rep(TRUE, nrow(count_mat))
  }
  genes_after_count_filter <- sum(keep)
  removed_by_count_filter <- total_genes_before - genes_after_count_filter

  # Safety check: if all genes would be filtered out, keep all with warning
  if (sum(keep) == 0) {
    warning("All genes would be filtered out! Keeping all genes instead.")
    keep <- rep(TRUE, nrow(count_mat))
    warning_msg <- "Filter too strict - showing all genes"
    genes_after_count_filter <- total_genes_before
    removed_by_count_filter <- 0
  } else {
    warning_msg <- NULL
  }

  count_mat <- count_mat[keep, , drop = FALSE]
  row_names <- row_names[keep]

  # Apply variance filter as secondary filter
  vf_mode <- variance_filter$mode %||% "percent"
  vf_value <- variance_filter$value %||% 100
  if (nrow(count_mat) > 0) {
    variances <- apply(count_mat, 1, var)

    if (vf_mode == "percent") {
      n_genes_to_keep <- round(nrow(count_mat) * as.numeric(vf_value) / 100)
      n_genes_to_keep <- max(1, n_genes_to_keep)
    } else {
      n_genes_to_keep <- as.numeric(vf_value)
    }

    keep_var <- rank(-variances) <= n_genes_to_keep

    count_mat <- count_mat[keep_var, , drop = FALSE]
    row_names <- row_names[keep_var]

    # Track variance filter results
    removed_by_variance_filter <- genes_after_count_filter - nrow(count_mat)
  } else {
    removed_by_variance_filter <- 0
  }

  # Set row names on count_mat for DESeq2 functions
  rownames(count_mat) <- row_names

  filter_details <- list()
  if (filter_type == "filter1") {
    filter_details[["count_filter"]] <- paste0(
      "Sum of counts ≥ ", gene_filter$min_count_sum,
      " (removed ", removed_by_count_filter, ")"
    )
  } else if (filter_type == "filter2") {
    filter_details[["count_filter"]] <- paste0(
      "≥", gene_filter$min_count_per_sample, " counts in ≥", gene_filter$min_samples, " samples",
      " (removed ", removed_by_count_filter, ")"
    )
  } else {
    filter_details[["count_filter"]] <- "No count filtering applied"
  }
  if (as.character(vf_value) != "100" && as.character(vf_value) != "") {
    if (vf_mode == "percent") {
      filter_details[["variance_filter"]] <- paste0(
        "Top ", vf_value, "% by variance",
        " (removed ", removed_by_variance_filter, ")"
      )
    } else {
      filter_details[["variance_filter"]] <- paste0(
        "Top ", vf_value, " genes by variance",
        " (removed ", removed_by_variance_filter, ")"
      )
    }
  } else {
    filter_details[["variance_filter"]] <- "No variance filtering applied"
  }

  filter_info <- list(
    total_genes = total_genes_before,
    filtered_genes = nrow(count_mat),
    removed_by_count_filter = removed_by_count_filter,
    removed_by_variance_filter = removed_by_variance_filter,
    filter_conditions = filter_details,
    warning = warning_msg
  )

  norm_counts <- apply_normalization(count_mat, normalization)
  transformed_data <- apply_transformation(count_mat, norm_counts, transformation, pseudocount)

  rownames(norm_counts) <- row_names
  rownames(transformed_data) <- row_names

  list(
    norm_counts = norm_counts,
    transformed_data = transformed_data,
    count_mat = count_mat,
    filter_info = filter_info,
    status = "success",
    error = NULL
  )
}

#' Apply normalization to count matrix
#' @param count_mat matrix of counts
#' @param normalization "deseq2", "tmm", "cpm", or "none"
#' @return Normalized counts matrix
apply_normalization <- function(count_mat, normalization) {
  if (normalization == "deseq2") {
    # DESeq2 size factor normalization
    deseq_counts <- round(count_mat)
    storage.mode(deseq_counts) <- "integer"
    dds <- DESeq2::DESeqDataSetFromMatrix(countData = deseq_counts,
                                          colData = data.frame(row.names = colnames(count_mat)),
                                          design = ~ 1)
    dds <- DESeq2::estimateSizeFactors(dds)
    DESeq2::counts(dds, normalized = TRUE)
  } else if (normalization == "tmm") {
    # TMM normalization via edgeR
    dge <- edgeR::DGEList(counts = count_mat)
    dge <- edgeR::calcNormFactors(dge, method = "TMM")
    edgeR::cpm(dge, normalized.lib.sizes = TRUE)
  } else if (normalization == "cpm") {
    # Simple CPM normalization
    edgeR::cpm(count_mat)
  } else {
    # No normalization
    count_mat
  }
}

#' Apply transformation to normalized counts
#' @param count_mat matrix of counts
#' @param norm_counts Normalized counts matrix
#' @param transformation "vst", "rlog", "log2", or "none"
#' @param pseudocount Pseudocount for log2 transformation
#' @return Transformed data matrix
apply_transformation <- function(count_mat, norm_counts, transformation, pseudocount = 1) {
  if (transformation == "vst") {
    # Variance stabilizing transformation
    deseq_counts <- round(count_mat)
    storage.mode(deseq_counts) <- "integer"
    dds <- DESeq2::DESeqDataSetFromMatrix(countData = deseq_counts,
                                          colData = data.frame(row.names = colnames(count_mat)),
                                          design = ~ 1)
    vsdata <- DESeq2::varianceStabilizingTransformation(dds, blind = FALSE)
    SummarizedExperiment::assay(vsdata)
  } else if (transformation == "rlog") {
    # Regularized log transformation
    deseq_counts <- round(count_mat)
    storage.mode(deseq_counts) <- "integer"
    dds <- DESeq2::DESeqDataSetFromMatrix(countData = deseq_counts,
                                          colData = data.frame(row.names = colnames(count_mat)),
                                          design = ~ 1)
    rld <- DESeq2::rlog(dds, blind = FALSE)
    SummarizedExperiment::assay(rld)
  } else if (transformation == "log2") {
    # Simple log2 transformation with pseudocount
    log2(norm_counts + pseudocount)
  } else {
    # No transformation, return normalized counts
    norm_counts
  }
}
