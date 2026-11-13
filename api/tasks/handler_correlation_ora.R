start_correlation_ora_task <- \(ctx) {
  task_data <- ctx$task_data
  task_item <- ctx$task_item
  analysis_id <- ctx$analysis_id
  metadata <- ctx$metadata
  config <- ctx$config
  analysis_key <- ctx$analysis_key
  current_analysis_dir <- ctx$analysis_dir
  collection <- task_data$collection
  value <- task_data$value
  organism <- metadata$organism %||% config$organism
  gene_list <- unique(as.character(unlist(task_data$gene_list, use.names = FALSE)))
  universe <- unique(as.character(unlist(task_data$universe, use.names = FALSE)))
  padj_threshold <- task_data$padj_threshold %||% 0.05
  #message(sprintf("[%s] Starting correlation ORA task: collection=%s, value=%s, genes=%s, universe=%s, padj_threshold=%s",
  #  Sys.time(), collection, value, length(gene_list), length(universe), padj_threshold
  #))

  fail_correlation_ora <- function(e) {
    metadata <- get_analysis(analysis_id)
    message(sprintf("[%s] Correlation ORA task failed", Sys.time()))
    update_analysis(metadata, list(
      status = "correlation_ora_failed",
      running = FALSE,
      correlation_ora_complete = FALSE,
      message = "Internal server error during correlation ORA calculation"
    ))
  }

  setup <- tryCatch({
    annotation_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = getDatabasePath(organism))
    on.exit(DBI::dbDisconnect(annotation_con), add = TRUE)
    correlation_df <- data.frame(padj = rep(1, length(gene_list)), row.names = gene_list)
    correlation_df <- enrichGeneAnnotations(correlation_df, annotation_con)

    task <- mirai::mirai({
      source(ORA_WORKER_SCRIPT_PATH)
      source("api/config/database_config.R")
      source("api/helpers/gsea_helpers.R")
      source("api/helpers/dataset_loader.R")
      result <- performORACalculation(
        correlation_df, collection, value, organism,
        padj_threshold = 1,
        selected_gene_ids = gene_list,
        universe_gene_ids = universe,
        result_padj_threshold = padj_threshold
      )
      list(success = TRUE, result = result, completed_at = Sys.time())
    },
    ORA_WORKER_SCRIPT_PATH = ORA_WORKER_SCRIPT_PATH,
    correlation_df = correlation_df,
    gene_list = gene_list,
    universe = universe,
    collection = collection,
    value = value,
    organism = organism,
    padj_threshold = padj_threshold,
    .timeout = 1800000
    )

    list(
      task = task,
      complete = function() {
        # Merge with the latest on-disk metadata
        metadata <- get_analysis(analysis_id)
        task_result <- mirai::call_mirai(task)$data
        if (is_error_value(task_result)) {
          fail_correlation_ora(task_result)
          return()
        }
        result <- task_result$result

        if (is_error_value(result)) {
          update_analysis(metadata, list(
            status = "correlation_ora_failed",
            running = FALSE,
            correlation_ora_complete = FALSE,
            message = "Internal server error during correlation ORA calculation"
          ))
          message(sprintf("[%s] Correlation ORA task failed", Sys.time()))
        } else {
          results_path <- file.path(current_analysis_dir, "correlation_ora_results.fst")
          if (nrow(result) == 0) {
            unlink(results_path)
            update_analysis(metadata, list(
              status = "correlation_ora_complete",
              running = FALSE,
              correlation_ora_complete = TRUE,
              message = "Correlation ORA completed, but no significant gene sets were found"
            ))
          } else {
            write_session_fst(result, results_path, analysis_key)
            update_analysis(metadata, list(
              status = "correlation_ora_complete",
              running = FALSE,
              correlation_ora_complete = TRUE,
              message = "Correlation ORA completed successfully"
            ))
          }
          message(sprintf("[%s] Correlation ORA task completed successfully", Sys.time()))
        }
      }
    )
  }, error = function(e) {
    fail_correlation_ora(e)
    NULL
  })

  if (is.null(setup)) {
    liteq::ack(task_item)
    return(FALSE)
  }
  list(task = setup$task, task_item = task_item, analysis_id = analysis_id,
       complete = setup$complete, fail = fail_correlation_ora, analysis_key = analysis_key)
}
