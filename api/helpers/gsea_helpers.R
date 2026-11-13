# Normalize term name by removing prefixes and converting to lowercase
normalizeTermName <- \(termName) {
  normalizedTermName <- sub("GOBP_|GOCC_|GOMF_|MP_|WP_|REACTOME_|HALLMARK_", "", termName)
  normalizedTermName <- tolower(gsub("_", " ", normalizedTermName))
  normalizedTermName
}

rankGenes <- function(de_results_df, ranking_method, de_results_vst_mat = NULL, gene_of_interest = NULL, correlation_method = "pearson", id_type = "ensembl") {
  if (ranking_method == "stat") {
    de_results_df$rank <- ifelse(
      is.na(de_results_df$stat),
      0,
      de_results_df$stat
    )
  } else if (ranking_method == "pvalue") {
    # Rank by -log10(p) so the most significant genes have the highest rank.
    # pmax() keeps exact zeros (underflowed p-values) finite instead of Inf.
    de_results_df$rank <- ifelse(
      is.na(de_results_df$pvalue),
      0,
      -log10(pmax(de_results_df$pvalue, .Machine$double.xmin))
    )
  } else if (ranking_method == "log2FoldChange") {
    de_results_df$rank <- ifelse(
      is.na(de_results_df$log2FC),
      0,
      de_results_df$log2FC
    )
  } else if (ranking_method == "-log10(pvalue) * log2FC_sign") {
    de_results_df$rank <- ifelse(
      is.na(de_results_df$log2FC) | is.na(de_results_df$pvalue),
      0,
      sign(de_results_df$log2FC) * (-log10(de_results_df$pvalue))
    )
  } else if (ranking_method == "gene_of_interest") {
    counts <- as.data.frame(de_results_vst_mat)
    # we used to filter genes here, but now we filter in the start_workers.R before calling this function
    counts <- t(counts)

    # Calculate correlations
    cor_list <- cor(counts[, gene_of_interest], counts, method = correlation_method)
    de_results_df$rank <- as.numeric(cor_list[1, rownames(de_results_df)])
  } else {
    stop("Invalid ranking method: ", ranking_method)
  }

  # Convert infinite ranking values to finite values
  max_ranking <- max(de_results_df$rank[is.finite(de_results_df$rank)])
  min_ranking <- min(de_results_df$rank[is.finite(de_results_df$rank)])
  if (is.finite(max_ranking) && is.finite(min_ranking)) {
    de_results_df$rank <- replace(de_results_df$rank, de_results_df$rank > max_ranking, max_ranking * 10)
    de_results_df$rank <- replace(de_results_df$rank, de_results_df$rank < min_ranking, min_ranking * 10)
  }

  # Order significant genes by rank (descending)
  ordered_sigs <- de_results_df[order(-de_results_df$rank), ]

  # Create gene list with ensembl IDs as names
  gene_list <- ordered_sigs[, "rank"]

  if (id_type == "ensembl") {
    names(gene_list) <- rownames(ordered_sigs)
  } else if (id_type == "entrez") {
    # Assign names from entrez, then fill missing ones with entrezNcbi
    names(gene_list) <- ifelse(!is.na(ordered_sigs$entrez),
      ordered_sigs$entrez,
      ordered_sigs$entrezNcbi
    )
  }

  na_indices <- is.na(names(gene_list))
  gene_list <- gene_list[!na_indices]

  # Remove duplicates, keeping first occurrence (highest rank)
  has_dups <- any(duplicated(names(gene_list)))
  if (has_dups) {
    unique_indices <- !duplicated(names(gene_list))
    gene_list <- gene_list[unique_indices]
  }

  # Store ensembl IDs as attribute when using entrez mapping
  # ordered_sigs is sorted by rank (descending), so first occurrence = highest rank
  # unique_indices keeps first occurrence, so ensembl_ids_kept aligns with gene_list
  if (id_type == "entrez") {
    ensembl_ids_kept <- rownames(ordered_sigs)[!na_indices]
    if (has_dups) {
      ensembl_ids_kept <- ensembl_ids_kept[unique_indices]
    }
    names(ensembl_ids_kept) <- names(gene_list)
    attr(gene_list, "ensembl_ids") <- ensembl_ids_kept
  }

  gene_list
}

# Order GSEA result rows for display: strongest absolute enrichment first,
# then the smallest adjusted p-value, then smaller gene sets, then the
# readable term name. NAs sort last, matching dplyr::arrange().
orderGSEAResults <- function(res_df) {
  res_df[order(
    -res_df$absNES,
    res_df$p.adjust,
    res_df$setSize,
    res_df$name
  ), , drop = FALSE]
}
