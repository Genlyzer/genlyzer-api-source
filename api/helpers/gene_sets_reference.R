LOCAL_GSEA_ORGANISMS <- c("Mus musculus", "Homo sapiens")
LOCAL_GSEA_COLLECTIONS <- c(
  "MSigDB_GO_BP+MF+CC",
  "MSigDB_GO_BP",
  "MSigDB_Hallmark",
  "MSigDB_Reactome",
  "MSigDB_KEGG_MEDICUS",
  "MSigDB_KEGG_LEGACY",
  "MSigDB_WikiPathways",
  "MSigDB_BIOCARTA"
)

validate_local_gene_set_request <- function(organism, collection_value) {
  if (is.null(organism) || length(organism) != 1L ||
        !is.character(organism) || !organism %in% LOCAL_GSEA_ORGANISMS) {
    stop("Organism must be 'Mus musculus' or 'Homo sapiens'.")
  }
  if (
    is.null(collection_value) || length(collection_value) != 1L ||
      !is.character(collection_value) ||
      !collection_value %in% LOCAL_GSEA_COLLECTIONS
  ) {
    stop("The selected gene-set collection is not available for local GSEA.")
  }

  parse_collection_value(collection_value)
}

build_local_gene_set_payload <- function(gene_sets, organism, collection_value) {
  term2gene <- data.table::as.data.table(gene_sets$TERM2GENE)
  term2name <- data.table::as.data.table(gene_sets$TERM2NAME)
  if (!all(c("term_id", "ensembl_gene_id") %in% names(term2gene)) ||
        !all(c("term_id", "term_name") %in% names(term2name))) {
    stop("gene_sets missing required columns.")
  }

  gene_members <- term2gene[
    !is.na(term_id) & !is.na(ensembl_gene_id),
    .(genes = paste(unique(as.character(ensembl_gene_id)), collapse = "/")),
    by = term_id
  ]
  metadata <- term2name[
    !is.na(term_id) & !is.na(term_name),
    .(
      term_id,
      term_name = as.character(term_name),
      description = as.character(term_description),
      external_id = as.character(external_id),
      systematic_name = as.character(systematic_name)
    )
  ]
  data.table::setorder(metadata, term_id)
  metadata <- metadata[, .SD[1L], by = term_id]
  pathways <- merge(gene_members, metadata, by = "term_id", all = FALSE, sort = FALSE)
  data.table::setorder(pathways, term_id)

  items <- lapply(seq_len(nrow(pathways)), function(i) {
    item <- pathways[i]
    id <- item$external_id
    if (is.na(id) || !nzchar(trimws(id))) {
      id <- if (!is.na(item$systematic_name) && nzchar(trimws(item$systematic_name))) {
        paste0("MSigDB:", item$systematic_name)
      } else {
        item$term_name
      }
    }
    list(
      id = id,
      term_id = as.character(item$term_id),
      name = item$term_name,
      description = if (is.na(item$description)) NULL else item$description,
      genes = item$genes
    )
  })

  list(
    organism = organism,
    collection = collection_value,
    pathways = items
  )
}
