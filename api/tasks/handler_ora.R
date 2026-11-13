# Monitor-side ORA task ------------------------------------------------------
# Runs over the stored DE result table and stores the significant gene sets.
# The ORA math lives in api/tasks/ora_calculation_task.R.

start_ora_task <- function(ctx) {
  task_data <- ctx$task_data
  task_item <- ctx$task_item
  analysis_id <- ctx$analysis_id
  metadata <- ctx$metadata
  config <- ctx$config
  analysis_key <- ctx$analysis_key

  collection <- task_data$collection
  value <- task_data$value
  organism <- config$organism
  padj_threshold <- task_data$padj_threshold %||% 0.05

  fail_ora <- function(e) {
    metadata <- get_analysis(analysis_id)
    message(sprintf("[%s] ORA task failed", Sys.time()))
    update_analysis(metadata, list(
      status = "ora_failed",
      running = FALSE,
      ora_complete = FALSE,
      message = "Internal server error during ORA calculation"
    ))
  }

  setup <- tryCatch({
    de_results_df <- load_de_results_df(analysis_id, metadata, analysis_key)
    if (is.null(de_results_df) || nrow(de_results_df) == 0) {
      update_analysis(metadata, list(
        status = "ora_failed",
        running = FALSE,
        ora_complete = FALSE,
        message = "DE results are unavailable. Please run DE analysis again before running ORA."
      ))
      liteq::ack(task_item)
      return(FALSE)
    }

    task <- mirai::mirai({
      source(ORA_WORKER_SCRIPT_PATH)
      source("api/config/database_config.R")
      source("api/helpers/gsea_helpers.R")
      source("api/helpers/dataset_loader.R")
      result <- performORACalculation(
        de_results_df, collection, value, organism, padj_threshold, analysis_id
      )
      list(success = TRUE, result = result, completed_at = Sys.time())
    },
    ORA_WORKER_SCRIPT_PATH = ORA_WORKER_SCRIPT_PATH,
    de_results_df = de_results_df,
    collection = collection,
    value = value,
    organism = organism,
    padj_threshold = padj_threshold,
    analysis_id = analysis_id,
    .timeout = 1800000
    )

    list(
      task = task,
      complete = function() {
        # Merge with the latest on-disk metadata
        metadata <- get_analysis(analysis_id)
        task_result <- mirai::call_mirai(task)$data
        if (is_error_value(task_result)) {
          fail_ora(task_result)
          return()
        }
        result <- task_result$result

        if (is_error_value(result)) {
          update_analysis(metadata, list(
            status = "ora_failed",
            running = FALSE,
            ora_complete = FALSE,
            message = "Internal server error during ORA calculation"
          ))
          message(sprintf("[%s] ORA task failed", Sys.time()))
        } else {
          if (nrow(result) == 0) {
            unlink(file.path(SESSION_DATA_DIR, analysis_id, "ora_results.fst"))
            update_analysis(metadata, list(
              status = "ora_complete",
              running = FALSE,
              ora_complete = TRUE,
              message = "ORA calculation completed, but no significant gene sets were found"
            ))
          } else {
            write_session_fst(result, file.path(SESSION_DATA_DIR, analysis_id, "ora_results.fst"), analysis_key)
            update_analysis(metadata, list(
              status = "ora_complete",
              running = FALSE,
              ora_complete = TRUE,
              message = "ORA calculation completed successfully"
            ))
          }
          message(sprintf("[%s] ORA task completed successfully", Sys.time()))
        }
      }
    )
  }, error = function(e) {
    fail_ora(e)
    NULL
  })

  if (is.null(setup)) {
    liteq::ack(task_item)
    return(FALSE)
  }
  list(task = setup$task, task_item = task_item, analysis_id = analysis_id,
       complete = setup$complete, fail = fail_ora, analysis_key = analysis_key)
}
