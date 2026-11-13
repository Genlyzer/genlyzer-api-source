QUEUE_DIR <- Sys.getenv("QUEUE_DIR", unset = "api/queue_liteq")
QUEUE_DB_PATH <- file.path(QUEUE_DIR, "liteq.sqlite")

build_task_payload <- function(task_type, params_list) {
  id <- uuid::UUIDgenerate(n = 1, use.time = TRUE)
  list(
    id = id,
    task_type = task_type,
    # The id travels inside the message as well: workers ignore it, but it
    # lets the queue be inspected (and a duplicate submit recognised) without
    # a side table mapping message rows to task ids.
    data = jsonlite::toJSON(c(list(id = id), params_list), auto_unbox = TRUE)
  )
}

push_to_queue <- function(task_item) {
  dir.create(QUEUE_DIR, recursive = TRUE, showWarnings = FALSE)
  queue <- liteq::ensure_queue("de_analysis", db = QUEUE_DB_PATH)

  params <- tryCatch(jsonlite::fromJSON(task_item$data), error = function(e) NULL)
  analysis_id <- params$analysis_id
  if (!is.null(analysis_id)) {
    existing <- tryCatch(
      queue_pending_task_id(analysis_id, task_item$task_type, QUEUE_DB_PATH),
      error = function(e) NULL
    )
    if (!is.null(existing) && nzchar(existing)) {
      message(sprintf("Duplicate %s task not queued", task_item$task_type))
      return(existing)
    }
  }

  liteq::publish(queue, title = task_item$task_type, message = task_item$data)
  message(sprintf("%s task queued", task_item$task_type))
  task_item$id
}

queue_de_analysis_task <- function(analysis_id, params = list()) {
  payload_params <- c(list(analysis_id = analysis_id), params)

  task_item <- build_task_payload("DE_ANALYSIS", payload_params)
  push_to_queue(task_item)
}

queue_gsea_analysis_task <- function(analysis_id, collection, value, ranking_method = "stat",
                                      gene_of_interest = NULL, correlation_method = "pearson",
                                      padj_threshold = 0.25) {
  payload_params <- list(
    analysis_id = analysis_id,
    collection = collection,
    value = value,
    ranking_method = ranking_method,
    gene_of_interest = gene_of_interest,
    correlation_method = correlation_method,
    padj_threshold = padj_threshold
  )

  task_item <- build_task_payload("GSEA_ANALYSIS", payload_params)
  push_to_queue(task_item)
}

queue_ora_analysis_task <- function(analysis_id, collection, value, padj_threshold = 0.05) {
  payload_params <- list(
    analysis_id = analysis_id,
    collection = collection,
    value = value,
    padj_threshold = padj_threshold
  )

  task_item <- build_task_payload("ORA_ANALYSIS", payload_params)
  push_to_queue(task_item)
}

queue_correlation_ora_analysis_task <- function(analysis_id, collection, value,
                                                gene_list, universe,
                                                padj_threshold = 0.05) {
  payload_params <- list(
    analysis_id = analysis_id,
    collection = collection,
    value = value,
    gene_list = gene_list,
    universe = universe,
    padj_threshold = padj_threshold
  )

  task_item <- build_task_payload("CORRELATION_ORA_ANALYSIS", payload_params)
  push_to_queue(task_item)
}

queue_explore_analysis_task <- function(analysis_id, params) {
  payload_params <- c(list(analysis_id = analysis_id), params)

  task_item <- build_task_payload("EXPLORE_ANALYSIS", payload_params)
  push_to_queue(task_item)
}
