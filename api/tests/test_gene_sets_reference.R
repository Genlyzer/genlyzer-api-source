#!/usr/bin/env Rscript

if (!file.exists("api/helpers/gene_sets_reference.R")) {
  cat("ERROR: run from the API repository root\n")
  quit(status = 1)
}

library(data.table)
source("api/helpers/validation.R")
source("api/helpers/gene_sets_reference.R")

checks <- 0L
check <- function(description, condition) {
  if (!isTRUE(condition)) {
    cat("[FAIL] ", description, "\n", sep = "")
    quit(status = 1)
  }
  checks <<- checks + 1L
  cat("[PASS] ", description, "\n", sep = "")
}

request <- validate_local_gene_set_request("Mus musculus", "MSigDB_Hallmark")
check("local GSEA accepts a supported organism and collection",
      identical(request$collection, "MSigDB_Hallmark"))
check("local GSEA rejects unsupported organisms",
      inherits(try(validate_local_gene_set_request("Arabidopsis thaliana", "MSigDB_Hallmark"), silent = TRUE), "try-error"))
check("local GSEA rejects collections outside its whitelist",
      inherits(try(validate_local_gene_set_request("Mus musculus", "MSigDB_HP"), silent = TRUE), "try-error"))

payload <- build_local_gene_set_payload(
  list(
    TERM2GENE = data.table(
      term_id = c("GO:1", "GO:1", "GO:1", "GO:2"),
      ensembl_gene_id = c("ENSMUSG1", "ENSMUSG1", "ENSMUSG2", "ENSMUSG3")
    ),
    TERM2NAME = data.table(
      term_id = c("GO:1", "GO:2"),
      term_name = c("HALLMARK_CELL_CYCLE", "custom_term"),
      term_description = c(NA_character_, "description"),
      external_id = c(NA_character_, "EXT:2"),
      systematic_name = c("HALLMARK_CELL_CYCLE", NA_character_)
    )
  ),
  "Mus musculus",
  "MSigDB_Hallmark"
)
check("gene-set payload contains selected organism and collection",
      identical(payload$organism, "Mus musculus") &&
        identical(payload$collection, "MSigDB_Hallmark"))
check("gene-set members are unique and identifiers use database metadata",
      identical(payload$pathways[[1]]$genes, "ENSMUSG1/ENSMUSG2") &&
        identical(payload$pathways[[1]]$id, "MSigDB:HALLMARK_CELL_CYCLE") &&
        identical(payload$pathways[[2]]$id, "EXT:2"))
check("missing descriptions remain null",
      is.null(payload$pathways[[1]]$description))
wire_payload <- jsonlite::toJSON(payload, auto_unbox = TRUE, na = "null")
decoded_payload <- jsonlite::fromJSON(wire_payload, simplifyVector = FALSE)
check("gene-set payload preserves pathway arrays and member strings in JSON",
      length(decoded_payload$pathways) == 2L &&
        identical(decoded_payload$pathways[[1]]$genes, "ENSMUSG1/ENSMUSG2"))

cat(sprintf("\nGene-set reference helper checks passed: %d\n", checks))
