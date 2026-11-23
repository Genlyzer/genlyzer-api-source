#!/usr/bin/env Rscript
# ============================================================================
# Advanced-design config validation
#
# Ported from the retired api/test_workflow.R script. These are focused unit
# checks for validate_config()'s advanced_design branch: condition-column
# resolution, reference/comparison levels, covariates, and blocking columns.
#
# Usage: Rscript api/tests/test_config_validation.R   (from the repo root)
# ============================================================================

if (!file.exists("api/helpers/validation.R")) {
  cat("ERROR: run from the repo root (api/helpers/validation.R not found)\n")
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

test_root <- tempfile("genlyzer-config-test-")
dir.create(test_root, recursive = TRUE)
SESSION_DATA_DIR <- file.path(test_root, "sessions")
SESSION_DECRYPTED_TMP_DIR <- file.path(test_root, "ram-staging")
dir.create(SESSION_DATA_DIR, recursive = TRUE)
dir.create(SESSION_DECRYPTED_TMP_DIR, recursive = TRUE)

source("api/helpers/error_handling.R")
source("api/helpers/file_handling.R")
source("api/helpers/encrypted_fst.R")
source("api/helpers/validation.R")

n_pass <- 0L
failures <- character()
check <- function(desc, cond) {
  ok <- isTRUE(cond)
  if (ok) n_pass <<- n_pass + 1L else failures <<- c(failures, desc)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", desc))
  invisible(ok)
}

# ---- fixture ----------------------------------------------------------------
counts <- data.frame(
  gene_id = c("ENSG00000123456", "ENSG00000234567", "ENSG00000345678"),
  control_1 = c(100, 200, 50),
  control_2 = c(120, 250, 45),
  control_3 = c(150, 220, 55),
  treated_1 = c(50, 100, 200),
  treated_2 = c(60, 110, 220),
  treated_3 = c(40, 90, 190),
  stringsAsFactors = FALSE
)

base_sheet <- data.frame(
  sample_id = c("control_1", "control_2", "control_3",
                "treated_1", "treated_2", "treated_3"),
  condition = c("Control", "Control", "Control",
                "Treated", "Treated", "Treated"),
  batch = c("batch1", "batch1", "batch1", "batch2", "batch2", "batch2"),
  patient_id = c("patient1", "patient2", "patient3",
                 "patient1", "patient2", "patient3"),
  stringsAsFactors = FALSE
)

base_config <- list(
  organism = "Homo sapiens",
  name_column = "gene_id",
  sample_columns = c("control_1", "control_2", "control_3",
                     "treated_1", "treated_2", "treated_3"),
  de_parameters = list(
    model = "DESeq2",
    gene_filter = list(type = "filter1", min_count_sum = 10),
    reference_samples = c("control_1", "control_2", "control_3"),
    comparison_samples = c("treated_1", "treated_2", "treated_3"),
    alpha = 0.05
  )
)

new_analysis <- function(sheet = base_sheet, write_sheet = TRUE) {
  analysis_id <- uuid::UUIDgenerate()
  session_dir <- file.path(SESSION_DATA_DIR, analysis_id)
  dir.create(session_dir, recursive = TRUE)
  write_session_fst(counts, get_counts_file_path(analysis_id))
  if (write_sheet && !is.null(sheet)) {
    write_session_fst(sheet, get_sample_sheet_path(analysis_id))
  }
  analysis_id
}

with_advanced <- function(advanced) {
  cfg <- base_config
  cfg$de_parameters$advanced_design <- advanced
  cfg
}

# Run validate_config() and return NULL on success or the problem title on
# failure, so each case asserts the intended validation branch.
validation_error <- function(cfg, sheet = base_sheet, write_sheet = TRUE) {
  analysis_id <- new_analysis(sheet, write_sheet)
  err <- tryCatch(
    {
      validate_config(cfg, analysis_id)
      NULL
    },
    error = function(e) e
  )
  if (is.null(err)) return(NULL)
  title <- err$response$body$title
  if (is.null(title)) conditionMessage(err) else title
}

# ---- valid advanced designs -------------------------------------------------
check("valid advanced design (no covariates)",
      is.null(validation_error(with_advanced(list(
        condition_column = "condition",
        reference_level = "Control",
        comparison_level = "Treated"
      )))))

check("valid advanced design with covariates + blocking",
      is.null(validation_error(with_advanced(list(
        condition_column = "condition",
        reference_level = "Control",
        comparison_level = "Treated",
        covariates = list("batch"),
        blocking_column = "patient_id"
      )))))

# ---- design cleaning: constant and rank-deficient variables ------------------
design_sheet <- data.frame(
  sample = c("control_1", "control_2", "control_3", "control_4",
             "treated_1", "treated_2", "treated_3", "treated_4"),
  condition = rep(c("control", "treated"), each = 4),
  treatment = rep(c("mock", "heat stress"), each = 4),
  temperature_c = rep(c(22, 37), each = 4),
  tissue = rep("rosette leaf", 8),
  time_hours = rep(3, 8),
  replicate = rep(1:4, 2),
  batch = rep(c("A", "A", "B", "B"), 2),
  stringsAsFactors = FALSE
)

confounded <- check_collinearity(
  design_sheet, "condition", "control", "treated",
  c("treatment", "temperature_c"), "batch"
)
check("condition-confounded covariates are dropped, blocking retained",
      all(c("treatment", "temperature_c") %in% confounded$collinear) &&
        identical(confounded$constant, character(0)) &&
        !("batch" %in% confounded$dropped))

constant <- check_collinearity(
  design_sheet, "condition", "control", "treated",
  c("tissue", "time_hours"), ""
)
check("constant covariates are reported as constant",
      all(c("tissue", "time_hours") %in% constant$constant) &&
        identical(constant$collinear, character(0)))

redundant <- check_collinearity_with_condition(
  design_sheet,
  factor(design_sheet$condition, levels = c("control", "treated")),
  c("replicate"),
  "batch"
)
check("redundant covariate is dropped and blocking retained",
      identical(redundant$collinear, "replicate") &&
        !("batch" %in% redundant$dropped))

combined <- check_collinearity(
  design_sheet, "condition", "control", "treated",
  c("treatment", "temperature_c", "replicate"), "batch"
)
check("multiple confounded covariates are all dropped, blocking retained",
      all(c("treatment", "temperature_c", "replicate") %in% combined$collinear) &&
        !("batch" %in% combined$dropped))

# validate_config() must persist the derived drop fields, not just compute them.
validated_config <- function(cfg, sheet = design_sheet) {
  analysis_id <- new_analysis(sheet, write_sheet = TRUE)
  validate_config(cfg, analysis_id)
}

validated <- validated_config(with_advanced(list(
  condition_column = "condition",
  reference_level = "control",
  comparison_level = "treated",
  covariates = list("treatment", "temperature_c", "tissue"),
  blocking_column = "batch"
)))
check("validated config persists collinear + constant drops",
      all(c("treatment", "temperature_c") %in%
            validated$de_parameters$advanced_design$collinear_dropped) &&
        "tissue" %in% validated$de_parameters$advanced_design$constant_dropped)

validated_no_condition <- validated_config(with_advanced(list(
  condition_column = "",
  covariates = list("batch")
)))
check("validated config normalizes empty condition fields away",
      is.null(validated_no_condition$de_parameters$advanced_design$condition_column) &&
        is.null(validated_no_condition$de_parameters$advanced_design$reference_level))

# ---- error paths ------------------------------------------------------------
check("advanced design without sample sheet is rejected",
      identical(
        validation_error(with_advanced(list(
          condition_column = "condition",
          reference_level = "Control",
          comparison_level = "Treated"
        )), write_sheet = FALSE),
        "Advanced design requires a sample sheet"
      ))

check("unknown condition column is rejected",
      identical(
        validation_error(with_advanced(list(
          condition_column = "nonexistent",
          reference_level = "Control",
          comparison_level = "Treated"
        ))),
        "Condition column not found"
      ))

single_level_sheet <- base_sheet
single_level_sheet$condition <- "OnlyGroup"
check("condition column with one level is rejected",
      identical(
        validation_error(with_advanced(list(
          condition_column = "condition",
          reference_level = "OnlyGroup",
          comparison_level = "Other"
        )), sheet = single_level_sheet),
        "Condition column must have at least 2 levels"
      ))

check("missing reference_level is rejected",
      identical(
        validation_error(with_advanced(list(
          condition_column = "condition",
          comparison_level = "Treated"
        ))),
        "Missing reference_level"
      ))

check("missing comparison_level is rejected",
      identical(
        validation_error(with_advanced(list(
          condition_column = "condition",
          reference_level = "Control"
        ))),
        "Missing comparison_level"
      ))

check("unknown reference level is rejected",
      identical(
        validation_error(with_advanced(list(
          condition_column = "condition",
          reference_level = "NonExistent",
          comparison_level = "Treated"
        ))),
        "Reference level not found"
      ))

check("unknown comparison level is rejected",
      identical(
        validation_error(with_advanced(list(
          condition_column = "condition",
          reference_level = "Control",
          comparison_level = "NonExistent"
        ))),
        "Comparison level not found"
      ))

check("reference == comparison is rejected",
      identical(
        validation_error(with_advanced(list(
          condition_column = "condition",
          reference_level = "Control",
          comparison_level = "Control"
        ))),
        "Reference and comparison levels must be different"
      ))

check("unknown covariate column is rejected",
      identical(
        validation_error(with_advanced(list(
          condition_column = "condition",
          reference_level = "Control",
          comparison_level = "Treated",
          covariates = list("nonexistent_cov")
        ))),
        "Covariate columns not found"
      ))

check("unknown blocking column is rejected",
      identical(
        validation_error(with_advanced(list(
          condition_column = "condition",
          reference_level = "Control",
          comparison_level = "Treated",
          blocking_column = "nonexistent_block"
        ))),
        "Blocking column not found"
      ))

unique_block_sheet <- base_sheet
unique_block_sheet$unique_id <- paste0("id", seq_len(nrow(unique_block_sheet)))
check("blocking column with all-unique values is rejected",
      identical(
        validation_error(with_advanced(list(
          condition_column = "condition",
          reference_level = "Control",
          comparison_level = "Treated",
          blocking_column = "unique_id"
        )), sheet = unique_block_sheet),
        "Blocking column has all unique values"
      ))

unlink(test_root, recursive = TRUE, force = TRUE)

total <- n_pass + length(failures)
cat(sprintf("\nConfig validation tests: %d/%d checks passed\n", n_pass, total))
if (length(failures)) {
  cat("Failed checks:\n")
  for (f in failures) cat("  -", f, "\n")
  quit(status = 1)
}
cat("PASS: advanced-design config validation\n")
