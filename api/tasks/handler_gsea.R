# Monitor-side GSEA task -----------------------------------------------------
# Ranks the stored DE results, submits the GSEA calculation to a mirai
# worker and stores the result. The ranking/GSEA math lives in
# gsea_calculation_task.R.

start_gsea_task <- function(ctx) {
  task_data <- ctx$task_data
  task_item <- ctx$task_item
  analysis_id <- ctx$analysis_id
  metadata <- ctx$metadata
  config <- ctx$config
  analysis_key <- ctx$analysis_key
  collection <- task_data$collection
  value <- task_data$value
  organism <- config$organism
  ranking_method <- task_data$ranking_method %||% "stat"
  gene_of_interest <- task_data$gene_of_interest %||% NULL
  correlation_method <- task_data$correlation_method %||% "pearson"
  padj_threshold <- task_data$padj_threshold %||% 0.25

  fail_gsea <- function(e) {
    metadata <- get_analysis(analysis_id)
    message(sprintf("[%s] GSEA task failed", Sys.time()))
    update_analysis(metadata, list(
      status = "gsea_failed",
      running = FALSE,
      gsea_complete = FALSE,
      message = "Internal server error during GSEA calculation"
    ))
  }

  setup <- tryCatch({
    de_results_df <- load_de_results_df(analysis_id, metadata, analysis_key)
    if (is.null(de_results_df) || nrow(de_results_df) == 0) {
      update_analysis(metadata, list(
        status = "gsea_failed",
        running = FALSE,
        gsea_complete = FALSE,
        message = "Internal server error, please run the DE analysis again before running GSEA"
      ))
      message(sprintf("[%s] GSEA task failed: No DE results available", Sys.time()))
      liteq::ack(task_item)
      return(FALSE)
    }

    de_results_vst_mat <- NULL
    if (identical(ranking_method, "gene_of_interest")) {
      if (!(gene_of_interest %in% rownames(de_results_df))) {
        update_analysis(metadata, list(
          status = "gsea_failed",
          running = FALSE,
          gsea_complete = FALSE,
          message = sprintf("Gene of interest '%s' not found. Make sure it is present in the DE results", gene_of_interest)
        ))
        message(sprintf("[%s] GSEA task failed: Gene of interest not found in DE results", Sys.time()))
        liteq::ack(task_item)
        return(FALSE)
      }
      de_results_vst_mat <- load_de_results_vst_mat(analysis_id, metadata, analysis_key)
    }

    task <- mirai::mirai({
      source(GSEA_WORKER_SCRIPT_PATH)
      source("api/config/database_config.R")
      source("api/helpers/gsea_helpers.R")
      source("api/helpers/logging.R")
      source("api/helpers/dataset_loader.R")
      gene_list <- rankGenes(de_results_df, ranking_method, de_results_vst_mat, gene_of_interest, correlation_method, id_type = "ensembl")
      result <- performGSEACalculation(de_results_df, gene_list, collection, value, organism, analysis_id, padj_threshold)
      list(success = TRUE, result = result, gene_list = gene_list, completed_at = Sys.time())
    },
    GSEA_WORKER_SCRIPT_PATH = GSEA_WORKER_SCRIPT_PATH,
    de_results_df = de_results_df,
    de_results_vst_mat = de_results_vst_mat,
    collection = collection,
    value = value,
    organism = organism,
    ranking_method = ranking_method,
    gene_of_interest = gene_of_interest,
    correlation_method = correlation_method,
    padj_threshold = padj_threshold,
    analysis_id = analysis_id,
    .timeout = 1800000  # 30 min timeout for GSEA task
    )

    list(
      task = task,
      complete = function() {
        # Merge with the latest on-disk metadata
        metadata <- get_analysis(analysis_id)
        task_result <- mirai::call_mirai(task)$data
        if (is_error_value(task_result)) {
          fail_gsea(task_result)
          return()
        }

        ranked_gene_list <- task_result$gene_list
        result <- task_result$result

        if (is_error_value(result)) {
          update_analysis(metadata, list(
            status = "gsea_failed",
            running = FALSE,
            gsea_complete = FALSE,
            message = "Internal server error during GSEA calculation"
          ))
          message(sprintf("[%s] GSEA task failed", Sys.time()))
        } else {
          if (nrow(result) == 0) {
            unlink(file.path(SESSION_DATA_DIR, analysis_id, c("gsea_results.fst", "gsea_rankings.fst")))
            update_analysis(metadata, list(
              status = "gsea_complete",
              running = FALSE,
              gsea_complete = TRUE,
              message = "GSEA calculation completed, but no significant gene sets were found"
            ))
            message(sprintf("[%s] GSEA Task completed successfully, but no significant gene sets were found", Sys.time()))
          } else {
            update_analysis(metadata, list(
              status = "gsea_complete",
              running = FALSE,
              gsea_complete = TRUE,
              message = "GSEA calculation completed successfully"
            ))
            save_gsea_results(analysis_id, result, analysis_key)
            save_gsea_rankings(analysis_id, ranked_gene_list, analysis_key)
            message(sprintf("[%s] GSEA Task completed successfully", Sys.time()))
          }
        }
      }
    )
  }, error = function(e) {
    fail_gsea(e)
    NULL
  })

  if (is.null(setup)) {
    liteq::ack(task_item)
    return(FALSE)
  }
  list(task = setup$task, task_item = task_item, analysis_id = analysis_id,
       complete = setup$complete, fail = fail_gsea, analysis_key = analysis_key)
}
