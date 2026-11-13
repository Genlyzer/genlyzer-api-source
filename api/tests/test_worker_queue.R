#!/usr/bin/env Rscript
# ============================================================================
# Regression: the queue monitor must dispatch tasks concurrently.
#
# The bug this pins down is silent: a monitor that waits for each mirai before
# consuming the next message still completes every analysis, just one after the
# other, so one long GSEA stalls every other user without any error.
#
# Stub tasks (Sys.sleep) replace the real DE/GSEA calculations: the property
# under test is the ordering of dispatches in monitor_queue(), not the math.
# Runtime ~2 s, no databases, no API server.
#
# Usage: Rscript api/tests/test_worker_queue.R   (from the repo root, or with
#        the repo root as the working directory)
# Exit status 0 = all checks passed.
# ============================================================================

if (!file.exists("api/tasks/start_workers.R")) {
  if (file.exists("../api/tasks/start_workers.R")) {
    setwd("..")
  } else {
    cat("ERROR: run from the repo root (api/tasks/start_workers.R not found)\n")
    quit(status = 1)
  }
}

if (!requireNamespace("mirai", quietly = TRUE) ||
    !requireNamespace("liteq", quietly = TRUE)) {
  cat("SKIP: mirai/liteq not installed\n")
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

elapsed <- function(from, to) as.numeric(difftime(to, from, units = "secs"))

fresh_queue <- function() {
  db <- tempfile(fileext = ".sqlite")
  list(db = db, queue = liteq::ensure_queue("de_analysis", db = db))
}

# A stub queue handler: consumes a real liteq message (so ack semantics are
# exercised) and runs a sleep task on a real mirai daemon.
stub_handler <- function(state, fail_in_complete = FALSE) {
  list(state = state, handler = function(queue) {
    item <- liteq::try_consume(queue)
    if (is.null(item)) return(NULL)
    params <- jsonlite::fromJSON(item$message)
    seconds <- if (is.null(params$seconds)) 0 else as.numeric(params$seconds)

    state$started <- c(state$started, as.numeric(Sys.time()))
    task <- mirai::mirai({
      Sys.sleep(seconds)
      "done"
    }, seconds = seconds)

    list(
      task = task,
      task_item = item,
      analysis_id = params$analysis_id,
      complete = function() {
        mirai::call_mirai(task)$data
        if (fail_in_complete) stop("completion handler failed")
        state$completed <- c(state$completed, as.numeric(Sys.time()))
      },
      fail = function(e) {
        state$failed_called <- TRUE
        state$fail_message <- conditionMessage(e)
      }
    )
  })
}

mirai::daemons(2)
on.exit(mirai::daemons(0), add = TRUE)

# ---------------------------------------------------------------------------
# 1. Two tasks must start together, and a third must take the first free slot.
#    Durations are distinct so "started before the other finished" is
#    unambiguous: 0.3 s (slot A), 1.0 s (slot B), then 0.1 s waiting in line.
# ---------------------------------------------------------------------------
cat("\n-- concurrent dispatch --\n")
q1 <- fresh_queue()
stub <- stub_handler(new.env(parent = emptyenv()))
stub_env <- stub$state
assign("start_queue_task", stub$handler, envir = globalenv())

for (task in list(
  list(analysis_id = "a", seconds = 0.3),
  list(analysis_id = "b", seconds = 1.0),
  list(analysis_id = "c", seconds = 0.1)
)) {
  liteq::publish(q1$queue, title = "DE_ANALYSIS",
                 message = jsonlite::toJSON(task, auto_unbox = TRUE))
}

t0 <- Sys.time()
monitor_queue(
  q1$queue, max_in_flight = 2,
  stop_when = function(pending) {
    length(pending) == 0 && length(stub_env$started) == 3 &&
      length(stub_env$completed) == 3
  },
  poll_interval = 0.02
)
total <- elapsed(t0, Sys.time())

started <- stub_env$started
completed <- stub_env$completed
dispatch_gap <- started[2] - started[1]

cat(sprintf("dispatches: %s | completions: %s | total %.2fs\n",
            paste(sprintf("%.2f", started - started[1]), collapse = ", "),
            paste(sprintf("%.2f", completed - started[1]), collapse = ", "),
            total))

# The second task is dispatched in the same loop iteration as the first, not
# after the first one resolves (that was the serialising bug).
check("second task dispatched without waiting for the first",
      length(started) >= 2 && dispatch_gap < 0.2)
check("third task started while the long task was still running",
      length(started) == 3 && started[3] < completed[2])
check("third task started only after a slot was freed",
      length(started) == 3 && started[3] >= completed[1])
# The reaper must notice the freed slot promptly. liteq::try_consume() can
# block ~20 s on an empty-but-busy queue, so a stalled monitor shows up here.
check("freed slot reused within one poll interval",
      length(started) == 3 && started[3] - completed[1] < 0.5)
check("all three tasks completed", length(completed) == 3)
check("two in flight, not three: total time bounded by the long task",
      total < 2.0)

counts <- queue_status_counts(q1$db)
check("queue drained after concurrent run (acked, not left WORKING/FAILED)",
      counts$ready == 0L && counts$working == 0L && counts$failed == 0L)

# ---------------------------------------------------------------------------
# 1b. The per-analysis queue state drives the "queued / running" label: READY
#     means waiting for a worker, WORKING means a worker claimed it.
# ---------------------------------------------------------------------------
cat("\n-- per-analysis queue state --\n")
q_state <- fresh_queue()
liteq::publish(q_state$queue, title = "GSEA_ANALYSIS",
               message = jsonlite::toJSON(
                 list(id = "first-task-id", analysis_id = "analysis-1"),
                 auto_unbox = TRUE))
check("READY message reports queued",
      identical(queue_task_state("analysis-1", q_state$db), "queued"))
item <- liteq::try_consume(q_state$queue)
check("WORKING message reports running",
      identical(queue_task_state("analysis-1", q_state$db), "running"))

# A duplicate submit leaves a second READY row behind the WORKING one; the
# analysis is running, so the label must not flip back to "queued".
duplicate_id <- "dup-task-id"
liteq::publish(q_state$queue, title = "GSEA_ANALYSIS",
               message = jsonlite::toJSON(
                 list(id = duplicate_id, analysis_id = "analysis-1"), auto_unbox = TRUE))
check("a duplicate READY row does not hide a running task",
      identical(queue_task_state("analysis-1", q_state$db), "running"))
check("queue_pending_task_id finds the waiting duplicate",
      identical(queue_pending_task_id("analysis-1", "GSEA_ANALYSIS", q_state$db),
                duplicate_id))
check("queue_pending_task_id ignores other task types",
      is.null(queue_pending_task_id("analysis-1", "ORA_ANALYSIS", q_state$db)))
check("unknown analysis has no queue state",
      is.null(queue_task_state("analysis-2", q_state$db)))

# A locked queue must report "unknown" (NA), never "not queued" (NULL): the API
# then omits the field and the client keeps its current label instead of
# falling back to the stale *_queued status.
lock_con <- DBI::dbConnect(RSQLite::SQLite(), q_state$db)
DBI::dbExecute(lock_con, "BEGIN EXCLUSIVE")
locked <- queue_task_state("analysis-1", q_state$db)
DBI::dbExecute(lock_con, "COMMIT")
DBI::dbDisconnect(lock_con)
check("locked queue reports unknown, not absent", identical(locked, NA_character_))

liteq::ack(item)
remaining <- liteq::try_consume(q_state$queue)
if (!is.null(remaining)) liteq::ack(remaining)
check("acked message has no queue state",
      is.null(queue_task_state("analysis-1", q_state$db)))

# ---------------------------------------------------------------------------
# 2. A completion handler that errors must still ack the message: the failure
#    is recorded on the analysis, replaying it would just fail again (and is
#    how FAILED rows used to pile up).
# ---------------------------------------------------------------------------
cat("\n-- failing completion is acked, not replayed --\n")
q2 <- fresh_queue()
stub2 <- stub_handler(new.env(parent = emptyenv()), fail_in_complete = TRUE)
stub2_env <- stub2$state
assign("start_queue_task", stub2$handler, envir = globalenv())

liteq::publish(q2$queue, title = "DE_ANALYSIS",
               message = jsonlite::toJSON(list(analysis_id = "x", seconds = 0.05),
                                          auto_unbox = TRUE))

monitor_queue(
  q2$queue, max_in_flight = 1,
  stop_when = function(pending) {
    length(pending) == 0 && isTRUE(stub2_env$failed_called)
  },
  poll_interval = 0.02
)

check("completion error reaches the handle's fail()",
      isTRUE(stub2_env$failed_called) &&
        grepl("completion handler failed", stub2_env$fail_message, fixed = TRUE))
counts2 <- queue_status_counts(q2$db)
check("failed task message is acked (queue empty, no FAILED row)",
      counts2$ready == 0L && counts2$working == 0L && counts2$failed == 0L)

cat("\n")
if (length(failures) == 0L) {
  cat(sprintf("PASS: %d checks\n", n_pass))
  quit(status = 0)
} else {
  cat(sprintf("FAIL: %d passed, %d failed\n", n_pass, length(failures)))
  cat(paste0("  - ", failures, collapse = "\n"), "\n")
  quit(status = 1)
}
