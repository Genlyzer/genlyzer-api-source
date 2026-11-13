#!/usr/bin/env Rscript
# ============================================================================
# Regression: every mirai daemon must have the analysis libraries preloaded
# before it runs a task.
#
# The bug this pins down is order-dependent and only shows up on the daemon
# that happens to miss the preload: DE fails with
# `could not find function "metadata"` because SummarizedExperiment/S4Vectors
# was never attached there. start_worker_pool_and_monitor() therefore uses
# mirai::everywhere(), which the dispatcher gates so that no task starts
# before the preload has completed on *all* daemons.
#
# The tasks below are launched immediately after the pool starts (no sleep),
# which is the window where a fire-and-forget preload would race.
#
# Usage: Rscript api/tests/test_worker_preload.R   (from the repo root)
# Exit status 0 = all checks passed. Runtime ~10 s.
# ============================================================================

if (!file.exists("api/tasks/start_workers.R")) {
  if (file.exists("../api/tasks/start_workers.R")) {
    setwd("..")
  } else {
    cat("ERROR: run from the repo root (api/tasks/start_workers.R not found)\n")
    quit(status = 1)
  }
}

if (!requireNamespace("mirai", quietly = TRUE)) {
  cat("SKIP: mirai not installed\n")
  quit(status = 0)
}

# Source the worker definitions without starting a pool or the monitor.
Sys.setenv(GENLYZER_NO_AUTOSTART = "1")
source("api/tasks/start_workers.R")

n_pass <- 0L
failures <- character()
check <- function(desc, cond) {
  ok <- isTRUE(cond)
  if (ok) n_pass <<- n_pass + 1L else failures <<- c(failures, desc)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", desc))
  invisible(ok)
}

queue_db_path <- file.path(tempdir(), "worker-preload-test", "liteq.sqlite")
on.exit({
  mirai::daemons(0)
  unlink(dirname(queue_db_path), recursive = TRUE, force = TRUE)
}, add = TRUE)

pool <- start_worker_pool_and_monitor(n_workers = 2L, queue_db_path = queue_db_path)
check("pool reports two workers", identical(pool$n_workers, 2L))

# Launch more tasks than daemons immediately, so each daemon runs at least one
# task in the startup window.
tasks <- lapply(seq_len(4L), function(i) {
  mirai::mirai({
    c(
      metadata = exists("metadata", mode = "function"),
      summarized_experiment = "SummarizedExperiment" %in% loadedNamespaces(),
      deseq2 = "DESeq2" %in% loadedNamespaces(),
      data_table = "data.table" %in% loadedNamespaces()
    )
  })
})
results <- lapply(tasks, function(task) task[])

every_daemon <- function(flag) {
  all(vapply(results, function(r) isTRUE(r[[flag]]), logical(1)))
}

check("metadata() is available on every daemon", every_daemon("metadata"))
check("SummarizedExperiment is loaded on every daemon", every_daemon("summarized_experiment"))
check("DESeq2 is loaded on every daemon", every_daemon("deseq2"))
check("data.table is loaded on every daemon", every_daemon("data_table"))

cat("\n")
if (length(failures) == 0L) {
  cat(sprintf("PASS: %d checks\n", n_pass))
  quit(status = 0)
}
cat(sprintf("FAIL: %d check(s) failed\n", length(failures)))
for (f in failures) cat("  - ", f, "\n", sep = "")
quit(status = 1)
