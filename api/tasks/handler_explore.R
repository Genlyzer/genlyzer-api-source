# Monitor-side explore task --------------------------------------------------
# Submits filtering / normalization / transformation to a mirai worker and
# stores the transformed matrix plus the summary. The math lives in
# explore_data_task.R.

start_explore_task <- function(ctx) {
  task_data <- ctx$task_data
  task_item <- ctx$task_item
  analysis_id <- ctx$analysis_id
  metadata <- ctx$metadata
  config <- ctx$config
  analysis_key <- ctx$analysis_key
  current_analysis_dir <- ctx$analysis_dir

  fail_explore <- function(e) {
    metadata <- get_analysis(analysis_id)
    message(sprintf("[%s] Explore task failed", Sys.time()))
    update_analysis(metadata, list(
      status = "explore_failed",
      running = FALSE,
      explore_complete = FALSE,
      message = paste("Exploration failed:", describe_task_error(e))
    ))
  }

  setup <- tryCatch({
    current_counts_file_path <- file.path(current_analysis_dir, COUNTS_FILE_NAME)

    if (!file.exists(current_counts_file_path)) {
      update_analysis(metadata, list(
        status = "explore_failed",
        running = FALSE,
        explore_complete = FALSE,
        message = "Count data not found for this analysis"
      ))
      message(sprintf("[%s] Explore task failed: count data missing", Sys.time()))
      liteq::ack(task_item)
      return(FALSE)
    }

    name_column <- task_data$name_column
    sample_columns <- as.character(unlist(task_data$sample_columns))
    organism <- task_data$organism %||% config$organism %||% "Mus musculus"
    explore_parameters <- task_data$explore_parameters

    if (is.null(name_column) || length(name_column) == 0 || length(sample_columns) == 0) {
      update_analysis(metadata, list(
        status = "explore_failed",
        running = FALSE,
        explore_complete = FALSE,
        message = "Exploration requires name_column and sample_columns"
      ))
      message(sprintf("[%s] Explore task failed: missing name_column/sample_columns", Sys.time()))
      liteq::ack(task_item)
      return(FALSE)
    }

    count_matrix <- load_explore_count_matrix(current_counts_file_path, name_column, sample_columns, analysis_key)

    message("Starting explore task in Mirai worker...")

    task <- mirai::mirai({
      source(EXPLORE_WORKER_SCRIPT_PATH)
      result <- perform_explore_data_transformation(count_matrix, explore_parameters)
      list(success = TRUE, result = result, completed_at = Sys.time())
    },
    EXPLORE_WORKER_SCRIPT_PATH = EXPLORE_WORKER_SCRIPT_PATH,
    count_matrix = count_matrix,
    explore_parameters = explore_parameters,
    .timeout = 1800000  # 30 min timeout for explore task
    )

    list(
      task = task,
      complete = function() {
        # Merge with the latest on-disk metadata
        metadata <- get_analysis(analysis_id)
        task_result <- mirai::call_mirai(task)$data
        if (is_error_value(task_result)) {
          fail_explore(task_result)
          return()
        }
        resultValue <- task_result$result

        if (is_error_value(resultValue)) {
          fail_explore(resultValue)
          return()
        }
        if (!is.null(resultValue$status) && resultValue$status == "error") {
          update_analysis(metadata, list(
            status = "explore_failed",
            running = FALSE,
            explore_complete = FALSE,
            message = resultValue$error %||% "Exploration failed"
          ))
          message(sprintf("[%s] Explore calculation failed", Sys.time()))
        } else {
          # Enrich transformed data with gene annotations
          con <- DBI::dbConnect(RSQLite::SQLite(), dbname = getDatabasePath(organism))
          on.exit(DBI::dbDisconnect(con), add = TRUE)
          transformed_df <- as.data.frame(resultValue$transformed_data)
          colnames(transformed_df) <- colnames(resultValue$transformed_data)
          transformed_df <- enrichGeneAnnotations(transformed_df, con)
          # fst does not persist row names: keep gene IDs as an explicit column
          transformed_df <- cbind(gene_id = rownames(transformed_df), transformed_df)
          rownames(transformed_df) <- NULL

          # Save first: if saving fails, the handler marks the analysis
          # explore_failed instead of leaving explore_complete with missing files
          save_explore_results(analysis_id, resultValue, transformed_df, analysis_key)

          # Mirror DEMO results into the shared content-hash cache. Private analyses run
          # with use_cache = FALSE and never enter this branch.
          explore_meta_updates <- list(
            status = "explore_complete",
            running = FALSE,
            explore_complete = TRUE,
            message = "Exploration completed successfully"
          )
          explore_cache_key <- task_data$cache_key %||% NULL
          if (isTRUE(task_data$use_cache) &&
                !is.null(explore_cache_key) && length(explore_cache_key) == 1L &&
                nzchar(as.character(explore_cache_key))) {
            explore_cache_key <- as.character(explore_cache_key)
            cache_store_files(explore_cache_key, current_analysis_dir, EXPLORE_RESULT_FILES)
            explore_meta_updates$explore_cache_key <- explore_cache_key
          }
          update_analysis(metadata, explore_meta_updates)
          message(sprintf("[%s] Explore task completed successfully", Sys.time()))
        }
      }
    )
  }, error = function(e) {
    fail_explore(e)
    NULL
  })

  if (is.null(setup)) {
    liteq::ack(task_item)
    return(FALSE)
  }
  list(task = setup$task, task_item = task_item, analysis_id = analysis_id,
       complete = setup$complete, fail = fail_explore, analysis_key = analysis_key)
}
