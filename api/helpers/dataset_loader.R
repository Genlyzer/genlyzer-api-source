# Dataset Loader - Helper functions for GSEA and ORA

getGeneSetsAndDescriptions <- function(con, collection, value, organism = "Mus musculus") {
  term2gene <- data.table(term_id = character(), ensembl_gene_id = character())
  term2name <- data.table(
    term_id = character(), term_name = character(), term_description = character(),
    external_id = character(), systematic_name = character()
  )
  msigdb_fields <- DBI::dbListFields(con, "gene_sets_msigdb")
  systematic_name_sql <- if ("systematic_name" %in% msigdb_fields) {
    "systematic_name"
  } else {
    "NULL AS systematic_name"
  }

  if (organism == "Mus musculus") {
    gene_sets_table <- "gene_sets_mgi"
    go_collection <- "MGI_GO"
  } else if (organism == "Homo sapiens") {
    gene_sets_table <- "gene_sets_goa"
    go_collection <- "GOA_GO"
  } else {
    stop("Unsupported organism: ", organism)
  }

  if (collection == go_collection) {
    if (value == "BP+MF+CC") {
      query <- sprintf("SELECT GO_ID AS term_id, ensembl_gene_id, go_name AS term_name, go_description AS term_description
                        FROM %s INDEXED BY %s
                        WHERE Qualifier NOT LIKE 'NOT%%'", gene_sets_table,
                        if (organism == "Mus musculus") "idx_gene_sets_mgi_go_id" else "idx_gene_sets_goa_go_id")
    } else {
      query <- sprintf("SELECT GO_ID AS term_id, ensembl_gene_id, go_name AS term_name, go_description AS term_description
                        FROM %s INDEXED BY %s
                        WHERE namespace = 'GO:%s' AND Qualifier NOT LIKE 'NOT%%'", gene_sets_table,
                        if (organism == "Mus musculus") "idx_gene_sets_mgi_namespace" else "idx_gene_sets_goa_namespace", toupper(value))
    }
  } else if (collection == "MSigDB_GO") {
    msigdb_collection <- if (organism == "Mus musculus") "M5" else "C5"
    if (value == "BP+MF+CC") {
      query <- sprintf("SELECT exact_source AS term_id, ensembl_gene_id, go_name AS term_name, go_description AS term_description,
                           exact_source AS external_id, %s
                     FROM gene_sets_msigdb INDEXED BY idx_gene_sets_msigdb_collection_subcollection
                     WHERE collection = '%s' AND subcollection IN ('GO:BP', 'GO:MF', 'GO:CC')", systematic_name_sql, msigdb_collection)
    } else if (value == "BP+MF+CC+HP") {
      query <- sprintf("SELECT exact_source AS term_id, ensembl_gene_id, go_name AS term_name, go_description AS term_description,
                           exact_source AS external_id, %s
                     FROM gene_sets_msigdb INDEXED BY idx_gene_sets_msigdb_collection_subcollection
                     WHERE collection = '%s'", systematic_name_sql, msigdb_collection)
    } else {
      query <- sprintf("SELECT exact_source AS term_id, ensembl_gene_id, go_name AS term_name, go_description AS term_description,
                           exact_source AS external_id, %s
                     FROM gene_sets_msigdb INDEXED BY idx_gene_sets_msigdb_collection_subcollection
                     WHERE collection = '%s' AND subcollection = 'GO:%s'", systematic_name_sql, msigdb_collection, toupper(value))
    }
  } else if (collection %in% c("MSigDB_MP", "MSigDB_HP", "MSigDB_GTRD", "MSigDB_MIRDB", "MSigDB_BIOCARTA", "MSigDB_CELL_TYPE", "MSigDB_Reactome", "MSigDB_WikiPathways", "MSigDB_KEGG_MEDICUS", "MSigDB_PID", "MSigDB_KEGG_LEGACY", "MSigDB_CGP", "MSigDB_MIR_MIRDB", "MSigDB_MIR_MIR_LEGACY", "MSigDB_TFT_GTRD", "MSigDB_TFT_TFT_LEGACY")) {
    coll <- switch(collection,
                   MSigDB_MP = "M5",
                   MSigDB_HP = "C5",
                   MSigDB_GTRD = "M3",
                   MSigDB_MIRDB = "M3",
                   MSigDB_BIOCARTA = if (organism == "Mus musculus") "M2" else "C2",
                   MSigDB_CELL_TYPE = "M8",
                   MSigDB_Reactome = if (organism == "Mus musculus") "M2" else "C2",
                   MSigDB_WikiPathways = if (organism == "Mus musculus") "M2" else "C2",
                   MSigDB_KEGG_MEDICUS = "C2",
                   MSigDB_PID = "C2",
                   MSigDB_KEGG_LEGACY = "C2",
                   MSigDB_CGP = if (organism == "Mus musculus") "M2" else "C2",
                   MSigDB_MIR_MIRDB = "C3",
                   MSigDB_MIR_MIR_LEGACY = "C3",
                   MSigDB_TFT_GTRD = "C3",
                   MSigDB_TFT_TFT_LEGACY = "C3")
    subcoll <- switch(collection,
                      MSigDB_MP = "MP",
                      MSigDB_HP = "HP",
                      MSigDB_GTRD = "GTRD",
                      MSigDB_MIRDB = "MIRDB",
                      MSigDB_BIOCARTA = "CP:BIOCARTA",
                      MSigDB_CELL_TYPE = "CELL_TYPE",
                      MSigDB_Reactome = "CP:REACTOME",
                      MSigDB_WikiPathways = "CP:WIKIPATHWAYS",
                      MSigDB_KEGG_MEDICUS = "CP:KEGG_MEDICUS",
                      MSigDB_PID = "CP:PID",
                      MSigDB_KEGG_LEGACY = "CP:KEGG_LEGACY",
                      MSigDB_CGP = "CGP",
                      MSigDB_MIR_MIRDB = "MIR:MIRDB",
                      MSigDB_MIR_MIR_LEGACY = "MIR:MIR_LEGACY",
                      MSigDB_TFT_GTRD = "TFT:GTRD",
                      MSigDB_TFT_TFT_LEGACY = "TFT:TFT_LEGACY")
    query <- sprintf("SELECT gene_set_id AS term_id, ensembl_gene_id, gene_set_name AS term_name,
                          COALESCE(go_description, description) AS term_description,
                          exact_source AS external_id, %s
                   FROM gene_sets_msigdb INDEXED BY idx_gene_sets_msigdb_collection_subcollection
                   WHERE collection = '%s' AND subcollection = '%s'",
                     systematic_name_sql, coll, subcoll)
  } else if (collection == "MSigDB_Hallmark") {
    hallmark_collection <- if (organism == "Mus musculus") "MH" else "H"
    query <- sprintf("SELECT gene_set_id AS term_id, ensembl_gene_id, gene_set_name AS term_name,
                          COALESCE(go_description, description) AS term_description,
                          exact_source AS external_id, %s
                   FROM gene_sets_msigdb INDEXED BY idx_gene_sets_msigdb_collection_subcollection
                   WHERE collection = '%s'", systematic_name_sql, hallmark_collection)
  } else {
    stop("Invalid collection")
  }

  result <- data.table()
  res <- NULL

  tryCatch({
    res <- dbSendQuery(con, query)
    while (!dbHasCompleted(res)) {
      chunk <- dbFetch(res, n = 50000)
      result <- rbindlist(list(result, as.data.table(chunk)))
    }
  }, error = function(e) stop("Failed to load gene-set annotations", call. = FALSE),
  finally = {
    if (!is.null(res)) dbClearResult(res)
  })

  if (nrow(result) > 0) {
    term2gene <- unique(result[
      !is.na(term_id) & trimws(as.character(term_id)) != "" &
        !is.na(ensembl_gene_id) & trimws(as.character(ensembl_gene_id)) != "",
      .(term_id, ensembl_gene_id)
    ])
    if (!"external_id" %in% names(result)) result[, external_id := as.character(term_id)]
    if (!"systematic_name" %in% names(result)) result[, systematic_name := NA_character_]
    term2name <- unique(result[
      !is.na(term_id) & trimws(as.character(term_id)) != "" &
        !is.na(term_name) & trimws(as.character(term_name)) != "",
      .(term_id, term_name, term_description, external_id, systematic_name)
    ])
  }

  list(TERM2GENE = term2gene, TERM2NAME = term2name)
}

# Enrich a data frame with gene annotation information from database
enrichGeneAnnotations <- \(data, con) {
  gene_ids <- rownames(data)

  if (is.null(gene_ids) || length(gene_ids) == 0) {
    message("Warning: No gene IDs found in data rownames")
    return(data)
  }

  gene_id_batches <- split(gene_ids, ceiling(seq_along(gene_ids) / 5000L))
  result <- do.call(rbind, lapply(gene_id_batches, function(batch) {
    quoted_ids <- paste(DBI::dbQuoteString(con, batch), collapse = ",")
    query <- paste(
      "SELECT ensembl_gene_id, external_gene_name, entrezgene_id,",
      "entrezgene_id_from_ncbi, gene_info FROM genes",
      "WHERE ensembl_gene_id IN (", quoted_ids, ")"
    )
    DBI::dbGetQuery(con, query)
  }))

  if (nrow(result) == 0) {
    message("Warning: No gene annotations found in database")
    data$symbol <- NA_character_
    data$geneInfo <- NA_character_
    data$entrez <- NA_character_
    data$entrezNcbi <- NA_character_
    orig_cols <- setdiff(names(data), c("symbol", "geneInfo", "entrez", "entrezNcbi"))
    data <- data[, c(orig_cols, "symbol", "geneInfo", "entrez", "entrezNcbi")]
    return(data)
  }

  # Match annotations to data
  data$symbol <- result$external_gene_name[match(gene_ids, result$ensembl_gene_id)]
  data$geneInfo <- result$gene_info[match(gene_ids, result$ensembl_gene_id)]
  data$entrez <- as.character(result$entrezgene_id[match(gene_ids, result$ensembl_gene_id)])
  data$entrezNcbi <- as.character(result$entrezgene_id_from_ncbi[match(gene_ids, result$ensembl_gene_id)])

  orig_cols <- setdiff(names(data), c("symbol", "geneInfo", "entrez", "entrezNcbi"))
  data <- data[, c(orig_cols, "symbol", "geneInfo", "entrez", "entrezNcbi")]

  data
}
