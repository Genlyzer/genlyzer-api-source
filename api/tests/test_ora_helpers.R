#!/usr/bin/env Rscript
# ============================================================================
# ORA request helpers
#
# run-correlation-ora delegates request validation and queue-payload
# preparation to api/helpers/ora_helpers.R. These checks pin down:
#   - collection-value parsing shared with run-gsea / run-ora,
#   - organism-specific Ensembl ID validation,
#   - the explore-universe (stale-result) check,
#   - significant-gene selection and its error paths.
#
# Usage: Rscript api/tests/test_ora_helpers.R   (from the repo root)
# ============================================================================

if (!file.exists("api/helpers/ora_helpers.R")) {
  cat("ERROR: run from the repo root (api/helpers/ora_helpers.R not found)\n")
  quit(status = 1)
}

required_packages <- c("RestRserve", "jsonlite", "fst", "uuid")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  cat("SKIP: missing packages: ", paste(missing_packages, collapse = ", "), "\n", sep = "")
  quit(status = 0)
}

suppressPackageStartupMessages({
  library(RestRserve)
  library(jsonlite)
  library(fst)
})

test_root <- tempfile("genlyzer-ora-test-")
dir.create(test_root, recursive = TRUE)
SESSION_DATA_DIR <- file.path(test_root, "sessions")
SESSION_DECRYPTED_TMP_DIR <- file.path(test_root, "ram-staging")
CACHE_DIR <- file.path(test_root, "cache")
Sys.setenv(GENLYZER_CACHE_DIR = CACHE_DIR)
dir.create(SESSION_DATA_DIR, recursive = TRUE)
dir.create(SESSION_DECRYPTED_TMP_DIR, recursive = TRUE)

source("api/helpers/error_handling.R")
source("api/helpers/file_handling.R")
source("api/helpers/encrypted_fst.R")
source("api/helpers/results_cache.R")
source("api/helpers/validation.R")
source("api/helpers/ora_helpers.R")

n_pass <- 0L
failures <- character()
check <- function(desc, cond) {
  ok <- isTRUE(cond)
  if (ok) n_pass <<- n_pass + 1L else failures <<- c(failures, desc)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", desc))
  invisible(ok)
}

# ---- collection parsing -----------------------------------------------------
mgi <- parse_collection_value("MGI_GO_BP")
check("parses MGI GO namespace",
      identical(mgi, list(collection = "MGI_GO", value = "BP")))
msig <- parse_collection_value("MSigDB_Hallmark")
check("parses singleton MSigDB collection",
      identical(msig, list(collection = "MSigDB_Hallmark", value = NULL)))
check("rejects unknown collection",
      inherits(try(parse_collection_value("nope"), silent = TRUE), "try-error"))

# ---- organism-specific ID patterns -----------------------------------------
check("mouse Ensembl pattern",
      identical(correlation_id_pattern("Mus musculus"),
                "^ENSMUSG[0-9]+([.][0-9]+)?$"))
check("human Ensembl pattern",
      identical(correlation_id_pattern("Homo sapiens"),
                "^ENSG[0-9]+([.][0-9]+)?$"))
check("arabidopsis AGI pattern",
      identical(correlation_id_pattern("Arabidopsis thaliana"),
                "^AT[1-5CM]G[0-9]{5}([.][0-9]+)?$"))
check("parses PlantReactome collection",
      identical(parse_collection_value("PlantReactome"),
                list(collection = "PlantReactome", value = NULL)))
check("parses WikiPathways collection",
      identical(parse_collection_value("WikiPathways"),
                list(collection = "WikiPathways", value = NULL)))
check("accepts Arabidopsis AGI count-matrix gene IDs",
      is.null(try(validate_gene_ids(c("AT1G01010", "at1g01020.1")), silent = TRUE)))
check("rejects malformed count-matrix gene IDs",
      inherits(try(validate_gene_ids(c("AT1G01010", "not-a-gene")), silent = TRUE),
               "try-error"))
check("unsupported organism raises",
      inherits(try(correlation_id_pattern("Rattus norvegicus"), silent = TRUE),
               "try-error"))

# ---- fixture ----------------------------------------------------------------
analysis_id <- "ora-test-analysis"
session_dir <- file.path(SESSION_DATA_DIR, analysis_id)
dir.create(session_dir, recursive = TRUE)
mouse_genes <- c(
  "ENSMUSG00000000001", "ENSMUSG00000000002", "ENSMUSG00000000003",
  "ENSMUSG00000000004"
)
write_session_fst(
  data.frame(gene_id = mouse_genes, stringsAsFactors = FALSE),
  file.path(session_dir, "explore_results.fst")
)
jsonlite::write_json(
  list(organism = "Mus musculus"),
  file.path(session_dir, "config.json"),
  auto_unbox = TRUE
)
analysis <- list(analysis_id = analysis_id)

body <- list(
  collection = "MGI_GO_BP",
  correlation_padj_threshold = 0.05,
  padj_threshold = 0.1,
  correlation_gene_ids = mouse_genes[1:3],
  correlation_padj_values = c(0.01, 0.2, 0.04)
)

task <- prepare_correlation_ora(body, analysis, analysis_id, NULL)
check("prepares collection for the queue",
      identical(task$collection, "MGI_GO") && identical(task$value, "BP"))
check("selects genes passing the correlation cutoff",
      identical(task$gene_list, mouse_genes[c(1, 3)]))
check("universe is every posted gene",
      identical(task$universe, mouse_genes[1:3]))
check("forwards the ORA padj threshold",
      identical(task$padj_threshold, 0.1))

# Explicit organism on the metadata also works (no config read needed).
analysis_with_organism <- analysis
analysis_with_organism$organism <- "Homo sapiens"
check("metadata organism is preferred over config",
      identical(
        analysis_organism(analysis_with_organism, analysis_id),
        "Homo sapiens"
      ))
check("config organism is used when metadata lacks it",
      identical(analysis_organism(analysis, analysis_id), "Mus musculus"))

# Arabidopsis fixture: lowercase AGI IDs with a transcript suffix are normalised
# to the uppercase, version-free form stored in the annotation database.
ath_analysis_id <- "ora-test-arabidopsis"
ath_session_dir <- file.path(SESSION_DATA_DIR, ath_analysis_id)
dir.create(ath_session_dir, recursive = TRUE)
ath_genes <- c("AT1G01010", "AT1G01020", "AT2G01010")
write_session_fst(
  data.frame(gene_id = ath_genes, stringsAsFactors = FALSE),
  file.path(ath_session_dir, "explore_results.fst")
)
jsonlite::write_json(
  list(organism = "Arabidopsis thaliana"),
  file.path(ath_session_dir, "config.json"),
  auto_unbox = TRUE
)
ath_analysis <- list(analysis_id = ath_analysis_id)
ath_body <- list(
  collection = "GOA_GO_BP",
  correlation_padj_threshold = 0.05,
  padj_threshold = 0.1,
  correlation_gene_ids = c("at1g01010.1", "AT2G01010"),
  correlation_padj_values = c(0.01, 0.2)
)
ath_task <- prepare_correlation_ora(ath_body, ath_analysis, ath_analysis_id, NULL)
check("normalises Arabidopsis AGI IDs for correlation ORA",
      identical(ath_task$universe, c("AT1G01010", "AT2G01010")))
check("selects significant Arabidopsis correlation genes",
      identical(ath_task$gene_list, "AT1G01010"))

# ---- error paths ------------------------------------------------------------
check_raises <- function(desc, expr) {
  check(desc, inherits(try(expr, silent = TRUE), "try-error"))
}

bad_collection <- body
bad_collection$collection <- "not_a_collection"
check_raises("unknown collection raises", {
  prepare_correlation_ora(bad_collection, analysis, analysis_id, NULL)
})

wrong_species <- body
wrong_species$correlation_gene_ids <- c("ENSG00000000001", mouse_genes[2])
wrong_species$correlation_padj_values <- c(0.01, 0.02)
check_raises("non-matching Ensembl IDs raise", {
  prepare_correlation_ora(wrong_species, analysis, analysis_id, NULL)
})

stale <- body
stale$correlation_gene_ids <- c(mouse_genes[1], "ENSMUSG00000000099")
stale$correlation_padj_values <- c(0.01, 0.02)
check_raises("genes outside the explore universe raise", {
  prepare_correlation_ora(stale, analysis, analysis_id, NULL)
})

no_significant <- body
no_significant$correlation_padj_values <- rep(0.9, 3)
check_raises("no significant genes raises", {
  prepare_correlation_ora(no_significant, analysis, analysis_id, NULL)
})

duplicate_ids <- body
duplicate_ids$correlation_gene_ids <- c(mouse_genes[1], mouse_genes[1])
duplicate_ids$correlation_padj_values <- c(0.01, 0.02)
check_raises("duplicate gene IDs raise", {
  prepare_correlation_ora(duplicate_ids, analysis, analysis_id, NULL)
})

mismatched_padj <- body
mismatched_padj$correlation_padj_values <- c(0.01)
check_raises("padj length mismatch raises", {
  prepare_correlation_ora(mismatched_padj, analysis, analysis_id, NULL)
})

bad_threshold <- body
bad_threshold$padj_threshold <- 2
check_raises("padj_threshold outside [0, 1] raises", {
  prepare_correlation_ora(bad_threshold, analysis, analysis_id, NULL)
})

unlink(test_root, recursive = TRUE, force = TRUE)

total <- n_pass + length(failures)
cat(sprintf("\nORA helper tests: %d/%d checks passed\n", n_pass, total))
if (length(failures)) {
  cat("Failed checks:\n")
  for (f in failures) cat("  -", f, "\n")
  quit(status = 1)
}
cat("PASS: collection parsing, correlation request validation, and gene selection\n")
