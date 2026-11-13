# Functions shared by task handlers

describe_task_error <- function(e) {
  tryCatch(conditionMessage(e), error = function(...) as.character(e)[1L])
}

# A distinct condition so finish_queue_task can tell "analysis is gone" apart
# from a calculation failure: a gone analysis has nothing to mark failed.
analysis_not_found <- function(analysis_id, metadata_path) {
  structure(
    class = c("analysis_not_found", "error", "condition"),
    list(
      message = sprintf("Analysis metadata not found for %s: %s",
                        analysis_id, metadata_path),
      call = NULL
    )
  )
}

# Read the analysis metadata
get_analysis <- function(analysis_id) {
  metadata_path <- file.path(SESSION_DATA_DIR, analysis_id, METADATA_FILE_NAME)
  if (!file.exists(metadata_path)) {
    stop(analysis_not_found(analysis_id, metadata_path))
  }
  tryCatch(
    jsonlite::read_json(metadata_path),
    error = function(e) {
      stop(sprintf("Failed to read analysis metadata for %s: %s",
                   analysis_id, describe_task_error(e)), call. = FALSE)
    }
  )
}


# Metadata fields that mark an analysis failed for the given task type.
task_failure_update <- function(task_type, msg = "Internal server error") {
  switch(
    task_type,
    DE_ANALYSIS = list(status = "de_failed", running = FALSE,
                       de_complete = FALSE, message = msg),
    GSEA_ANALYSIS = list(status = "gsea_failed", running = FALSE,
                         gsea_complete = FALSE, message = msg),
    ORA_ANALYSIS = list(status = "ora_failed", running = FALSE,
                        ora_complete = FALSE, message = msg),
    CORRELATION_ORA_ANALYSIS = list(status = "correlation_ora_failed", running = FALSE,
                                    correlation_ora_complete = FALSE, message = msg),
    EXPLORE_ANALYSIS = list(status = "explore_failed", running = FALSE,
                            explore_complete = FALSE, message = msg),
    list(status = "failed", running = FALSE, message = msg)
  )
}

finish_queue_task <- function(handle) {
  tryCatch(handle$complete(), error = function(e) {
    message(sprintf("[%s] Failed to finish task", Sys.time()))
    # A deleted/purged analysis has no metadata left to update: log and stop.
    if (inherits(e, "analysis_not_found")) return(invisible(NULL))
    tryCatch(handle$fail(e),
             error = function(e2) message("Could not mark task failed"))
  })
  # the task is done, successfully
  # or not. A failed task is reported through metadata
  # delete it from the queue
  tryCatch(
    liteq::ack(handle$task_item),
    error = function(e) message("Failed to acknowledge task")
  )
}
