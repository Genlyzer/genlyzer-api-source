library(mirai)
library(fst)
library(liteq)
library(jsonlite)

source("api/helpers/analysis_state.R")
source("api/config/database_config.R")
source("api/helpers/dataset_loader.R")
source("api/helpers/validation.R")

COUNTS_FILE_NAME <- "count_matrix.fst"
CONFIG_FILE_NAME <- "config.json"
METADATA_FILE_NAME <- "metadata.json"
SESSION_DATA_DIR <- Sys.getenv("SESSION_DATA_DIR", "api/session_data")
QUEUE_DB_PATH <- Sys.getenv("QUEUE_DB_PATH", "api/queue_liteq/liteq.sqlite")
CLEANUP_AGE_DAYS <- as.numeric(Sys.getenv("CLEANUP_AGE_DAYS", "7"))
# to run inside mirai daemons
DE_WORKER_SCRIPT_PATH <- "api/tasks/de_calculation_task.R"
GSEA_WORKER_SCRIPT_PATH <- "api/tasks/gsea_calculation_task.R"
ORA_WORKER_SCRIPT_PATH <- "api/tasks/ora_calculation_task.R"
EXPLORE_WORKER_SCRIPT_PATH <- "api/tasks/explore_data_task.R"


source("api/helpers/sample_sheet_helpers.R")
source("api/helpers/results_cache.R")
source("api/helpers/queue_stats.R")
source("api/helpers/encrypted_fst.R")
source("api/helpers/worker_key_channel.R")

WORKER_KEY_REGISTRY <- new_worker_key_registry()

# to run in the monitor process
#   worker_task.R  handle lifecycle shared by every task
#   worker_io.R    inputs and results on disk
#   handler_*.R    one task type each (context in, handle out)
source("api/tasks/worker_task.R")
source("api/tasks/worker_io.R")
source("api/tasks/handler_de.R")
source("api/tasks/handler_gsea.R")
source("api/tasks/handler_ora.R")
source("api/tasks/handler_correlation_ora.R")
source("api/tasks/handler_explore.R")

start_worker_pool_and_monitor <- function(n_workers = NULL, queue_db_path = "api/queue_liteq/liteq.sqlite") {
  if (is.null(n_workers)) {
    # One task may be CPU-bound on several cores (GSEA uses BiocParallel
    # forks), so half the cores seems to be safer default than detectCores() - 1:
    # the rest stays available for the API process.
    n_workers <- max(1L, min(4L, parallel::detectCores() %/% 2L))
  }

  queue_dir <- dirname(queue_db_path)
  dir.create(queue_dir, recursive = TRUE, showWarnings = FALSE)

  message(sprintf("Starting Mirai worker pool with %d local worker(s)", n_workers))

  # Launch local daemons.
  mirai::daemons(n_workers)

  message("Mirai worker pool started successfully")

  # Preload heavy libraries on every daemon.
  mirai::everywhere({
    library(dplyr)
    library(data.table)
    library(SummarizedExperiment)
    library(DESeq2)
    library(edgeR)
    library(limma)
    library(clusterProfiler)
    library(jsonlite)
    library(fst)
    library(RSQLite)
    library(DBI)
  })

  message(sprintf("Started mirai pool with %d worker(s)", n_workers))

  list(n_workers = n_workers)
}

# Consume one queued message and hand it to its task handler.
#
# A consumed liteq message is tracked as a handle while its mirai runs:
#   $task        mirai object, resolves when the daemon finishes
#   $task_item   liteq message, holds the queue lock until ack()
#   $analysis_id
#   $complete()  persist the result (called once the mirai has resolved)
#   $fail(e)     mark the analysis failed
#
# Each handler returns its handle without waiting for the mirai, so the monitor
# keeps MIRAI_N_WORKERS tasks in flight. The handler bodies live in
# api/tasks/handler_*.R; this function only builds the shared context and
# dispatches, so the per-task code stays independent.
start_queue_task <- function(queue) {
  task_item <- liteq::try_consume(queue)

  if (is.null(task_item)) return(NULL)

  # liteq stores the JSON payload in $message and the task type in $title.
  task_data <- jsonlite::fromJSON(task_item$message)
  analysis_id <- task_data$analysis_id
  task_type <- task_item$title

  message(sprintf("[%s] Starting %s task", Sys.time(), task_type))

  current_analysis_dir <- file.path(SESSION_DATA_DIR, analysis_id)
  current_config_file_path <- file.path(current_analysis_dir, CONFIG_FILE_NAME)
  current_metadata_file_path <- file.path(current_analysis_dir, METADATA_FILE_NAME)

  metadata <- tryCatch(
    jsonlite::read_json(current_metadata_file_path),
    error = function(e) NULL
  )
  if (is.null(metadata)) {
    # Nothing to update and nowhere to store a result: dropping the message is
    # better than leaving it WORKING (and later FAILED) forever.
    message(sprintf("[%s] Dropping %s task: metadata missing", Sys.time(), task_type))
    liteq::ack(task_item)
    return(FALSE)
  }

  # Config only exists once /config has been called; exploration can run on a
  # freshly uploaded analysis
  config <- if (file.exists(current_config_file_path)) {
    jsonlite::read_json(current_config_file_path)
  } else {
    list()
  }

  handle <- tryCatch({
    ctx <- list(
      task_item = task_item,
      task_data = task_data,
      analysis_id = analysis_id,
      metadata = metadata,
      config = config,
      analysis_key = if (isTRUE(metadata$demo)) NULL else analysis_id |> take_worker_analysis_key(WORKER_KEY_REGISTRY),
      analysis_dir = current_analysis_dir
    )

    handler <- switch(
      task_type,
      DE_ANALYSIS = start_de_task,
      GSEA_ANALYSIS = start_gsea_task,
      ORA_ANALYSIS = start_ora_task,
      CORRELATION_ORA_ANALYSIS = start_correlation_ora_task,
      EXPLORE_ANALYSIS = start_explore_task,
      NULL
    )

    if (is.null(handler)) {
      message(sprintf("[%s] Dropping unknown task type: %s", Sys.time(), task_type))
      liteq::ack(task_item)
      return(FALSE)
    }

    handler(ctx)
  }, error = function(e) {
    # A task that cannot even be prepared must not kill the monitor, and must
    # not stay WORKING forever (liteq would only requeue it after a crash).
    message(sprintf("[%s] Failed to start %s task", Sys.time(), task_type))
    tryCatch(
      update_analysis(metadata, task_failure_update(task_type)),
      error = function(e2) message("Could not mark analysis failed")
    )
    liteq::ack(task_item)
    FALSE
  })

  handle
}


# Dispatch/reap loop ---------------------------------------------------------
# Runs until stop_when(pending) is TRUE (the launcher never stops).
monitor_queue <- function(queue, max_in_flight,
                          stop_when = function(pending) FALSE,
                          poll_interval = 0.25,
                          janitor = NULL,
                          key_server = NULL) {
  pending <- list()
  last_purge <- Sys.time()

  while (!isTRUE(stop_when(pending))) {
    if (!is.null(key_server)) {
      tryCatch(
        poll_worker_key_handoffs(key_server, WORKER_KEY_REGISTRY),
        error = function(e) message("Worker key handoff failed")
      )
    }

    # Dispatch until every worker slot is busy or the queue is empty.
    while (length(pending) < max_in_flight) {
      ready <- tryCatch(
        queue_status_counts(queue$db, queue$name)$ready,
        error = function(e) NA_integer_
      )
      if (isTRUE(ready == 0L)) break

      handle <- start_queue_task(queue)
      if (is.null(handle)) break  # consumed by someone else / gone
      if (isFALSE(handle)) next   # message handled (failure recorded), try the next
      pending[[length(pending) + 1L]] <- handle
    }

    # Reap finished tasks.
    if (length(pending) > 0L) {
      finished <- which(!vapply(pending, function(h) mirai::unresolved(h$task), logical(1)))
      for (i in rev(finished)) {
        handle <- pending[[i]]
        finish_queue_task(handle)
        pending[[i]] <- NULL
      }
    }

    if (is.function(janitor) &&
          as.numeric(difftime(Sys.time(), last_purge, units = "hours")) >= 1) {
      tryCatch(janitor(), error = function(e) message("Janitor failed"))
      last_purge <- Sys.time()
    }

    Sys.sleep(poll_interval)
  }

  invisible(pending)
}

# Main execution. GENLYZER_NO_AUTOSTART lets the regression test source() this
# file for its definitions without starting a pool and monitor.
if (!interactive() && !nzchar(Sys.getenv("GENLYZER_NO_AUTOSTART"))) {
  # Get configuration from environment
  n_workers <- as.integer(Sys.getenv("MIRAI_N_WORKERS", ""))
  queue_db_path <- Sys.getenv("QUEUE_DB_PATH", "api/queue_liteq/liteq.sqlite")

  message("Starting worker pool with the following configuration:")
  message("Number of workers: ", ifelse(is.na(n_workers) || n_workers <= 0, "Auto-detect", n_workers))

  initialize_session_tmp_dir()

  # Bind the key channel before launching daemons so the port is listening for
  # the whole startup window (the API may hand off a key while workers boot).
  key_server <- new_worker_key_server()
  on.exit(close(key_server), add = TRUE)
  message(sprintf(
    "Worker key channel listening on tcp://%s:%d",
    WORKER_KEY_HOST, WORKER_KEY_PORT
  ))

  pool <- start_worker_pool_and_monitor(
    n_workers = if (is.na(n_workers) || n_workers <= 0) NULL else n_workers,
    queue_db_path = queue_db_path
  )
  max_in_flight <- pool$n_workers

  queue <- liteq::ensure_queue("de_analysis", db = queue_db_path)

  message(sprintf("Queue monitor running with up to %d concurrent task(s). Press Ctrl+C to stop.", max_in_flight))

  tryCatch({
    # Hourly janitor: expire cache entries and inactive sessions per the
    # privacy policy's N-day retention promise.
    janitor <- function() {
      purge_expired_results_cache()
      purge_expired_sessions()
      # FAILED is terminal in liteq
      purged <- purge_failed_queue_messages(queue_db_path)
      if (purged > 0L) message(sprintf("Purged %d failed queue message(s)", purged))
    }
    monitor_queue(queue, max_in_flight, janitor = janitor, key_server = key_server)
  }, interrupt = function(e) {
    message("\nShutting down worker pool and queue monitor...")
    mirai::daemons(0)
  })
}
