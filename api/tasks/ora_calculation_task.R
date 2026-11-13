performORACalculation <- function(de_results_df, collection, value, organism,
                                  padj_threshold = 0.05, analysis_id = NULL,
                                  selected_gene_ids = NULL, universe_gene_ids = NULL,
                                  result_padj_threshold = padj_threshold) {
  gene_ids <- rownames(de_results_df)
  if (is.null(gene_ids) || length(gene_ids) == 0) {
    stop("No gene identifiers are available for ORA.")
  }

  significant <- !is.na(de_results_df$padj) & de_results_df$padj <= padj_threshold
  gene_list <- if (is.null(selected_gene_ids)) {
    unique(as.character(gene_ids[significant]))
  } else {
    unique(as.character(selected_gene_ids))
  }
  gene_list <- gene_list[!is.na(gene_list) & nzchar(gene_list)]
  if (length(gene_list) == 0) {
    return(data.frame())
  }
  universe <- if (is.null(universe_gene_ids)) {
    unique(as.character(gene_ids))
  } else {
    unique(as.character(universe_gene_ids))
  }
  universe <- universe[!is.na(universe) & nzchar(universe)]

  term2name <- NULL
  if (identical(collection, "KEGG")) {
    if (!all(c("entrez", "entrezNcbi") %in% names(de_results_df))) {
      stop("KEGG ORA requires Entrez gene annotations.")
    }

    best_entrez <- as.character(de_results_df$entrez)
    missing_entrez <- is.na(best_entrez) | !nzchar(best_entrez)
    best_entrez[missing_entrez] <- as.character(de_results_df$entrezNcbi[missing_entrez])
    gene_list <- unique(best_entrez[significant])
    gene_list <- gene_list[!is.na(gene_list) & nzchar(gene_list)]
    universe <- unique(best_entrez)
    universe <- universe[!is.na(universe) & nzchar(universe)]
    if (length(gene_list) == 0 || length(universe) == 0) {
      return(data.frame())
    }

    ora <- clusterProfiler::enrichKEGG(
      gene = gene_list,
      universe = universe,
      organism = if (identical(organism, "Mus musculus")) "mmu" else "hsa",
      pvalueCutoff = 1,
      qvalueCutoff = 1,
      minGSSize = 10,
      maxGSSize = 500
    )
  } else {
    task_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = getDatabasePath(organism))
    on.exit(DBI::dbDisconnect(task_con), add = TRUE)

    data <- getGeneSetsAndDescriptions(task_con, collection, value, organism)
    term2gene <- data$TERM2GENE
    term2name <- data$TERM2NAME
    if (nrow(term2gene) == 0) {
      stop("The selected gene-set collection contains no genes.")
    }

    names(term2gene) <- c("TERM", "Gene")
    term2name <- term2name[, .(
      TERM = term_id,
      Description = normalizeTermName(term_name),
      Details = term_description
    )]

    ora <- clusterProfiler::enricher(
      gene = gene_list,
      universe = universe,
      TERM2GENE = term2gene[, c("TERM", "Gene")],
      TERM2NAME = term2name[, c("TERM", "Description")],
      pvalueCutoff = 1,
      qvalueCutoff = 1,
      pAdjustMethod = "BH",
      minGSSize = 5,
      maxGSSize = 2000
    )
  }
  if (is.null(ora)) {
    return(data.frame())
  }

  results <- as.data.frame(ora)
  if (nrow(results) == 0) {
    return(data.frame())
  }
  results <- results[!is.na(results$p.adjust) & results$p.adjust <= result_padj_threshold, , drop = FALSE]
  if (nrow(results) == 0) {
    return(data.frame())
  }

  results$geneRatio <- vapply(strsplit(as.character(results$GeneRatio), "/", fixed = TRUE), function(ratio) {
    if (length(ratio) != 2 || is.na(as.numeric(ratio[[2]])) || as.numeric(ratio[[2]]) == 0) {
      return(NA_real_)
    }
    as.numeric(ratio[[1]]) / as.numeric(ratio[[2]])
  }, numeric(1))
  results$size <- as.integer(sub("/.*", "", results$BgRatio))
  universe_size <- as.numeric(sub(".*/", "", results$BgRatio))
  results$richFactor <- results$Count / results$size
  results$FoldEnrichment <- results$geneRatio / (results$size / universe_size)

  if (is.null(term2name)) {
    results$Details <- NA_character_
  } else {
    results <- merge(
      results,
      term2name[, c("TERM", "Details")],
      by.x = "ID",
      by.y = "TERM",
      all.x = TRUE,
      sort = FALSE
    )
  }
  results$name <- results$ID
  symbols <- if ("symbol" %in% names(de_results_df)) {
    as.character(de_results_df$symbol)
  } else {
    rep(NA_character_, length(gene_ids))
  }
  if (identical(collection, "KEGG")) {
    best_entrez <- as.character(de_results_df$entrez)
    missing_entrez <- is.na(best_entrez) | !nzchar(best_entrez)
    best_entrez[missing_entrez] <- as.character(de_results_df$entrezNcbi[missing_entrez])
    valid <- !is.na(best_entrez) & nzchar(best_entrez)
    entrez_to_symbol <- setNames(symbols[valid], best_entrez[valid])
    entrez_to_ensembl <- setNames(gene_ids[valid], best_entrez[valid])
    matched_ids <- strsplit(as.character(results$geneID), "/", fixed = TRUE)
    results$gene_id_symbol <- vapply(matched_ids, function(ids) {
      symbols <- unique(na.omit(entrez_to_symbol[ids]))
      paste(symbols[nzchar(symbols)], collapse = "/")
    }, character(1))
    results$gene_id_ensembl <- vapply(matched_ids, function(ids) {
      ensembl <- unique(na.omit(entrez_to_ensembl[ids]))
      paste(ensembl[nzchar(ensembl)], collapse = "/")
    }, character(1))
  } else {
    ensembl_to_symbol <- setNames(symbols, gene_ids)
    results$gene_id_symbol <- vapply(strsplit(as.character(results$geneID), "/", fixed = TRUE), function(ids) {
      symbols <- unique(na.omit(ensembl_to_symbol[ids]))
      paste(symbols[nzchar(symbols)], collapse = "/")
    }, character(1))
    results$gene_id_ensembl <- as.character(results$geneID)
  }

  if (identical(collection, "KEGG")) {
    results <- results[order(-results$geneRatio, results$size, results$Description), , drop = FALSE]
  } else {
    results <- results[order(results$p.adjust, -results$geneRatio, results$size, results$name), , drop = FALSE]
  }
  results[, c(
    "name", "Description", "Details", "size", "Count", "pvalue", "p.adjust",
    "qvalue", "GeneRatio", "BgRatio", "geneRatio", "richFactor",
    "FoldEnrichment", "geneID", "gene_id_symbol", "gene_id_ensembl"
  )]
}
