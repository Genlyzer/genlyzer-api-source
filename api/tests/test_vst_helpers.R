#!/usr/bin/env Rscript
# ============================================================================
# VST response helpers
#
# GET /v1/analyses/{analysis_id}/de-results?vst=1 returns the stored
# transformed matrix plus sample labels. These checks pin down:
#   - reading the gene_id-prefixed fst back into a matrix,
#   - the optional ?genes= filter and its 1000-gene cap,
#   - reference/comparison sample labels (Unknown fallback),
#   - missing-matrix / no-matching-gene errors.
#
# Usage: Rscript api/tests/test_vst_helpers.R   (from the repo root)
# ============================================================================

if (!file.exists("api/helpers/vst_helpers.R")) {
  cat("ERROR: run from the repo root (api/helpers/vst_helpers.R not found)\n")
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

test_root <- tempfile("genlyzer-vst-test-")
dir.create(test_root, recursive = TRUE)
SESSION_DATA_DIR <- file.path(test_root, "sessions")
SESSION_DECRYPTED_TMP_DIR <- file.path(test_root, "ram-staging")
dir.create(SESSION_DATA_DIR, recursive = TRUE)
dir.create(SESSION_DECRYPTED_TMP_DIR, recursive = TRUE)

source("api/helpers/analysis_state.R")
source("api/helpers/error_handling.R")
source("api/helpers/file_handling.R")
source("api/helpers/encrypted_fst.R")
source("api/helpers/vst_helpers.R")

n_pass <- 0L
failures <- character()
check <- function(desc, cond) {
  ok <- isTRUE(cond)
  if (ok) n_pass <<- n_pass + 1L else failures <<- c(failures, desc)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", desc))
  invisible(ok)
}

analysis_id <- "vst-test-analysis"
session_dir <- file.path(SESSION_DATA_DIR, analysis_id)
dir.create(session_dir, recursive = TRUE)

vst_df <- data.frame(
  gene_id = c("gene1", "gene2", "gene3"),
  s1 = c(1, 4, 7),
  s2 = c(2, 5, 8),
  s3 = c(3, 6, 9),
  s4 = c(10, 11, 12),
  stringsAsFactors = FALSE
)
write_session_fst(vst_df, file.path(session_dir, "vst_mat.fst"))
jsonlite::write_json(
  list(de_parameters = list(
    reference_samples = c("s1", "s2"),
    comparison_samples = c("s3")
  )),
  file.path(session_dir, "config.json"),
  auto_unbox = TRUE
)

# ---- full payload -----------------------------------------------------------
payload <- vst_response_payload(analysis_id, session_dir)
check("all genes returned by default", identical(payload$gene_ids, vst_df$gene_id))
check("samples keep count-matrix order",
      identical(payload$samples, c("s1", "s2", "s3", "s4")))
check("reference/comparison/unknown labels",
      identical(payload$conditions, c("Group1", "Group1", "Group2", "Unknown")))
check("matrix rows preserve values",
      identical(payload$matrix[[2]], as.numeric(vst_df[2, -1])))

# ---- ?genes= filter ---------------------------------------------------------
filtered <- vst_response_payload(
  analysis_id, session_dir, genes_param = "gene3,gene1,missing"
)
check("?genes= keeps requested order and ignores unknown IDs",
      identical(filtered$gene_ids, c("gene3", "gene1")))
check("?genes= filters matrix rows",
      identical(filtered$matrix[[1]], as.numeric(vst_df[3, -1])))

# ---- cap --------------------------------------------------------------------
capped <- vst_response_payload(analysis_id, session_dir, max_genes = 2L)
check("default path honours the gene cap", identical(capped$gene_ids, c("gene1", "gene2")))
capped_request <- vst_response_payload(
  analysis_id, session_dir, genes_param = "gene1,gene2,gene3", max_genes = 2L
)
check("requested genes honour the cap",
      identical(capped_request$gene_ids, c("gene1", "gene2")))

# ---- error paths ------------------------------------------------------------
check("unknown genes raise 404", inherits(
  try(vst_response_payload(analysis_id, session_dir, genes_param = "nope"), silent = TRUE),
  "try-error"
))

missing_id <- "vst-missing-analysis"
dir.create(file.path(SESSION_DATA_DIR, missing_id), recursive = TRUE)
check("missing vst_mat.fst raises 404", inherits(
  try(vst_response_payload(missing_id, file.path(SESSION_DATA_DIR, missing_id)), silent = TRUE),
  "try-error"
))

# Missing config: matrix still returned, labels fall back to Unknown.
no_config_id <- "vst-no-config"
no_config_dir <- file.path(SESSION_DATA_DIR, no_config_id)
dir.create(no_config_dir, recursive = TRUE)
write_session_fst(vst_df, file.path(no_config_dir, "vst_mat.fst"))
no_config <- vst_response_payload(no_config_id, no_config_dir)
check("missing config falls back to Unknown labels",
      identical(no_config$conditions, rep("Unknown", 4L)))

unlink(test_root, recursive = TRUE, force = TRUE)

total <- n_pass + length(failures)
cat(sprintf("\nVST helper tests: %d/%d checks passed\n", n_pass, total))
if (length(failures)) {
  cat("Failed checks:\n")
  for (f in failures) cat("  -", f, "\n")
  quit(status = 1)
}
cat("PASS: VST matrix loading, gene filtering, and sample labels\n")
