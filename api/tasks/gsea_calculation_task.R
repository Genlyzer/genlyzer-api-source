performGSEACalculation <- function(de_results_df, gene_list, collection, value, organism, analysis_id = NULL, padj_threshold = 0.25) {
  # Fetch gene sets from DB
  task_con <- dbConnect(SQLite(), dbname = getDatabasePath(organism))
  on.exit(dbDisconnect(task_con), add = TRUE)
  data <- getGeneSetsAndDescriptions(task_con, collection, value, organism)
  TERM2GENE <- data$TERM2GENE
  TERM2NAME <- data$TERM2NAME

  setnames(TERM2GENE, c("term_id", "ensembl_gene_id"), c("TERM", "Gene"))
  TERM2NAME <- TERM2NAME[, .(
    TERM = term_id,
    Description = normalizeTermName(term_name),
    Details = term_description,
    external_id,
    systematic_name
  )]

  gs_res <- clusterProfiler::GSEA(
    geneList = gene_list,
    TERM2GENE = TERM2GENE[, c("TERM", "Gene")],
    TERM2NAME = TERM2NAME[, c("TERM", "Description")],
    pvalueCutoff = 1,
    # Keep clusterProfiler/fgsea defaults for the permutation count (nPerm /
    # nPermSimple = 1000). eps = 1e-10 is the documented default too
    eps = 1e-10,
    verbose = FALSE,
    minGSSize = 5,
    maxGSSize = 2000
  )

  res_df <- as.data.frame(gs_res)
  res_df <- res_df[which(res_df$p.adjust <= padj_threshold), ]

  if (is.null(gs_res) || nrow(res_df) == 0) {
    # No significant gene sets found for the given parameters
    return(data.frame())
  }

  term_metadata <- unique(TERM2NAME[, .(TERM, Details, external_id, systematic_name)])
  setorder(term_metadata, TERM, na.last = TRUE)
  term_metadata <- term_metadata[, .SD[1L], by = TERM]
  res_df <- merge(
    res_df,
    term_metadata,
    by.x = "ID",
    by.y = "TERM",
    all.x = TRUE
  )
  rownames(res_df) <- res_df$ID
  res_df$name <- res_df$Description
  res_df$Description <- res_df$Details
  res_df$id <- vapply(seq_len(nrow(res_df)), function(i) {
    external_id <- res_df$external_id[i]
    if (!is.na(external_id) && nzchar(trimws(external_id))) {
      return(external_id)
    }

    systematic_name <- res_df$systematic_name[i]
    if (!is.na(systematic_name) && nzchar(trimws(systematic_name))) {
      return(paste0("MSigDB:", systematic_name))
    }

    res_df$name[i]
  }, character(1))
  res_df$ID <- NULL
  res_df$Details <- NULL
  res_df$external_id <- NULL
  res_df$systematic_name <- NULL
  res_df$absNES <- abs(res_df$NES)

  # Sort results: strongest absolute enrichment first, then adjusted p-value.
  # The readable name is the final tie-break, matching the original monolith.
  res_df <- orderGSEAResults(res_df)

  names(res_df)[names(res_df) == "setSize"] <- "size"
  names(res_df)[names(res_df) == "enrichmentScore"] <- "ES"

  # Calculate geneRatio from core_enrichment
  res_df$geneRatio <- sapply(strsplit(as.character(res_df$core_enrichment), "/"), length) / res_df$size

  # Reorder columns
  col_order <- c("id", "name", "Description", "size", "ES",
                 setdiff(names(res_df), c("id", "name", "Description", "size", "ES")))
  res_df <- res_df[, col_order]

  ensembl_to_symbol <- setNames(de_results_df$symbol, rownames(de_results_df))
  convertEnsemblToGeneName <- \(ensembl_str) {
    ids <- strsplit(ensembl_str, "/", fixed = TRUE)[[1L]]
    syms <- ensembl_to_symbol[ids]
    syms <- syms[!is.na(syms)]
    if (length(syms) == 0) return(NA_character_)
    paste(unique(syms), collapse = "/")
  }
  res_df$core_enrichment_symbols <-
    vapply(res_df$core_enrichment, convertEnsemblToGeneName, character(1))

  tested_genes <- TERM2GENE[TERM2GENE$Gene %in% names(gene_list), .(TERM, Gene)]
  resolve_term_id <- function(term_label, leading_edge) {
    if (term_label %in% tested_genes$TERM) return(term_label)

    candidates <- unique(TERM2NAME$TERM[TERM2NAME$Description == term_label])
    if (length(candidates) == 1L) return(candidates[[1L]])
    if (length(candidates) == 0L) return(NA_character_)

    leading_edge_genes <- strsplit(leading_edge, "/", fixed = TRUE)[[1L]]
    overlaps <- vapply(candidates, function(candidate) {
      sum(leading_edge_genes %in% tested_genes$Gene[tested_genes$TERM == candidate])
    }, integer(1))
    best <- which(overlaps == max(overlaps))
    if (length(best) == 1L && overlaps[best] > 0L) candidates[best] else NA_character_
  }
  res_df$gene_set_genes <- vapply(seq_len(nrow(res_df)), function(i) {
    term_id <- resolve_term_id(res_df$name[i], res_df$core_enrichment[i])
    if (is.na(term_id)) return("")
    genes <- unique(as.character(tested_genes$Gene[tested_genes$TERM == term_id]))
    paste(genes, collapse = "/")
  }, character(1))

  res_df
}
