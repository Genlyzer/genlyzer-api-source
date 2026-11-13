#!/usr/bin/env Rscript
# ============================================================================
# Demo analysis seeding
#
# A demo is an ordinary analysis created with POST /analyses?demo=true and
# seeded from the bundled public counts + sample sheet. There is no shared
# "demo" session and no warmup queue task. These checks pin down:
#   - the demo query-flag parser,
#   - the seeded files/metadata/hashes,
#   - the unencrypted (no X-Analysis-Key) demo contract,
#   - demo-only content-hash cache key resolution.
#
# Usage: Rscript api/tests/test_demo.R   (from the repo root)
# ============================================================================

if (!file.exists("api/helpers/demo.R") ||
    !file.exists("api/demo-data/demo_counts.xls") ||
    !file.exists("api/demo-data/demo_sample_sheet.csv")) {
  cat("ERROR: run from the repo root (api/helpers/demo.R not found)\n")
  quit(status = 1)
}

required_packages <- c("RestRserve", "jsonlite", "dplyr", "fst", "data.table", "uuid", "readxl")
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
  library(dplyr)
  library(fst)
  library(data.table)
})

test_root <- tempfile("genlyzer-demo-test-")
dir.create(test_root, recursive = TRUE)
SESSION_DATA_DIR <- file.path(test_root, "sessions")
SESSION_DECRYPTED_TMP_DIR <- file.path(test_root, "ram-staging")
CACHE_DIR <- file.path(test_root, "cache")
Sys.setenv(GENLYZER_CACHE_DIR = CACHE_DIR)
dir.create(SESSION_DATA_DIR, recursive = TRUE)
dir.create(SESSION_DECRYPTED_TMP_DIR, recursive = TRUE)

source("api/helpers/analysis_state.R")
source("api/helpers/validation.R")
source("api/helpers/error_handling.R")
source("api/helpers/file_handling.R")
source("api/helpers/sample_sheet_helpers.R")
source("api/helpers/content_types.R")
source("api/helpers/encrypted_fst.R")
source("api/helpers/results_cache.R")
source("api/helpers/demo.R")

n_pass <- 0L
failures <- character()
check <- function(desc, cond) {
  ok <- isTRUE(cond)
  if (ok) n_pass <<- n_pass + 1L else failures <<- c(failures, desc)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", desc))
  invisible(ok)
}

# ---- query flag -------------------------------------------------------------
check("demo flag: absent -> false", !parse_demo_flag(NULL))
check("demo flag: true/1/yes -> true",
      parse_demo_flag("true") && parse_demo_flag("1") && parse_demo_flag("yes"))
check("demo flag: false/0 -> false",
      !parse_demo_flag("false") && !parse_demo_flag("0"))

# ---- "Cache results" flag ---------------------------------------------------
check("use_cache defaults to on", parse_use_cache_flag(list()))
check("use_cache=false is respected", !parse_use_cache_flag(list(use_cache = FALSE)))

# ---- normal analysis is not a demo -----------------------------------------
normal <- create_analysis()
check("normal analysis is not demo", !is_demo_analysis(normal$analysis_id))
check("normal analysis still requires a key",
      inherits(try(
        analysis_key_from_request(list(headers = list()), normal$analysis_id),
        silent = TRUE
      ), "try-error"))

# ---- seeded demo analysis ---------------------------------------------------
demo <- create_demo_analysis()
demo_id <- demo$analysis_id
demo_dir <- file.path(SESSION_DATA_DIR, demo_id)

check("demo analysis is marked demo", is_demo_analysis(demo_id))
check("demo metadata is configured",
      isTRUE(demo$demo) && isTRUE(demo$configured) &&
        isTRUE(demo$counts_uploaded))
check("demo metadata has demo dimensions",
      identical(as.integer(demo$sample_count), 8L) &&
        identical(as.integer(demo$gene_count), 59L))
check("demo count/sheet/config files were seeded",
      all(file.exists(file.path(demo_dir, c(
        "count_matrix.fst", "sample_sheet.fst", "config.json", "metadata.json"
      )))))
check("demo hashes recorded",
      nzchar(demo$counts_hash) && nzchar(demo$sample_sheet_hash))

cfg <- jsonlite::read_json(file.path(demo_dir, "config.json"))
check("demo config pins the public dataset structure",
      identical(cfg$name_column, "#NAME") &&
        identical(as.character(unlist(cfg$sample_columns)),
                  c("C1", "C2", "C3", "C4", "LKO1", "LKO2", "LKO3", "LKO4")))

sheet <- read_session_fst(file.path(demo_dir, "sample_sheet.fst"))
check("demo sample sheet is readable without a key",
      identical(nrow(sheet), 12L) && "condition" %in% names(sheet))
check("demo reads need no browser key",
      is.null(analysis_key_from_request(list(headers = list()), demo_id)))

# Cache keys are content-addressed: same demo inputs -> same key, while a
# changed analysis setting gives a different key. The shared cache must never
# be keyed without a demo's content hashes.
cache_key_a <- de_cache_key(demo$counts_hash, demo$sample_sheet_hash, cfg)
cache_key_b <- de_cache_key(demo$counts_hash, demo$sample_sheet_hash, cfg)
modified_cfg <- cfg
modified_cfg$de_parameters$alpha <- 0.1
check("demo cache key is deterministic", identical(cache_key_a, cache_key_b))
check("demo cache key depends on settings",
      !identical(cache_key_a, de_cache_key(demo$counts_hash, demo$sample_sheet_hash, modified_cfg)))

# ---- demo cache params ------------------------------------------------------
plan_miss <- demo_de_cache_params(demo, list(), cfg)
check("demo DE cache params compute a key when caching is on",
      isTRUE(plan_miss$use_cache) &&
        identical(plan_miss$cache_key, cache_key_a) && !plan_miss$hit)
check("demo DE cache params expose the key for the queue task",
      identical(plan_miss$use_cache, TRUE) &&
        identical(plan_miss$cache_key, cache_key_a))

plan_opt_out <- demo_de_cache_params(demo, list(use_cache = FALSE), cfg)
check("demo DE cache params honour use_cache=false",
      !plan_opt_out$use_cache && is.null(plan_opt_out$cache_key) &&
        !plan_opt_out$hit)

private_with_hashes <- normal
private_with_hashes$counts_hash <- demo$counts_hash
private_with_hashes$sample_sheet_hash <- demo$sample_sheet_hash
private_plan <- demo_de_cache_params(private_with_hashes, list(), cfg)
check("private DE cache params never use the shared cache",
      !private_plan$use_cache && is.null(private_plan$cache_key) &&
        !private_plan$hit)

explore_params <- list(
  organism = "Mus musculus",
  name_column = "#NAME",
  sample_columns = c("C1", "C2", "C3", "C4", "LKO1", "LKO2", "LKO3", "LKO4"),
  explore_parameters = list(transformation = "vst", normalization = "deseq2")
)
explore_plan <- demo_explore_cache_params(demo, list(), explore_params)
check("demo explore cache params compute a key when caching is on",
      isTRUE(explore_plan$use_cache) &&
        identical(explore_plan$cache_key,
                  explore_cache_key(demo$counts_hash, explore_params)) &&
        !explore_plan$hit)
check("demo explore cache params contain no queue task payload",
      !("task_params" %in% names(explore_plan)) &&
        identical(sort(names(explore_plan)),
                  sort(c("use_cache", "cache_key", "hit", "hit_analysis_meta"))))

explore_opt_out <- demo_explore_cache_params(
  demo, list(use_cache = FALSE), explore_params
)
check("demo explore cache params honour use_cache=false",
      !explore_opt_out$use_cache && is.null(explore_opt_out$cache_key) &&
        !explore_opt_out$hit)

dir.create(cache_dir_for(cache_key_a), recursive = TRUE)
plan_hit <- demo_de_cache_params(demo, list(), cfg)
check("demo DE cache params report a cache hit",
      isTRUE(plan_hit$hit) &&
        identical(plan_hit$hit_analysis_meta$status, "de_complete") &&
        identical(plan_hit$hit_analysis_meta$de_cache_key, cache_key_a))

check("demo results resolve to the shared cache",
      identical(
        analysis_results_dir(list(
          analysis_id = demo_id, demo = TRUE, de_cache_key = cache_key_a
        ), "de"),
        cache_dir_for(cache_key_a)
      ))
check("private results stay in the session directory",
      identical(
        analysis_results_dir(list(analysis_id = normal$analysis_id), "de"),
        file.path(SESSION_DATA_DIR, normal$analysis_id)
      ))

# Every call gets a fresh normal session, never a shared fixed id.
demo2 <- create_demo_analysis()
check("each demo call creates a fresh analysis",
      !identical(demo2$analysis_id, demo_id) && is_demo_analysis(demo2$analysis_id))

unlink(test_root, recursive = TRUE, force = TRUE)

total <- n_pass + length(failures)
cat(sprintf("\nDemo tests: %d/%d checks passed\n", n_pass, total))
if (length(failures)) {
  cat("Failed checks:\n")
  for (f in failures) cat("  -", f, "\n")
  quit(status = 1)
}
cat("PASS: demo seeding, demo-only cache keys, and keyless reads\n")
