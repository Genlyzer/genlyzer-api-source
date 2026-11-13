# Queue introspection (shared by the API and the worker monitor) -------------
# liteq's public API only exposes a total message count, which also includes
# terminal FAILED messages. Splitting by status is a plain SQL query on the
# queue table liteq maintains ("qq<queue name>").

queue_table_name <- function(queue_name = "de_analysis") {
  paste0("qq", queue_name)
}

queue_db_path_default <- function() {
  # Prefer the path the running process actually configured. QUEUE_DIR (and an
  # absolute path in the service units) is what server.R and queue.R resolve
  # QUEUE_DB_PATH from
  if (exists("QUEUE_DB_PATH", envir = .GlobalEnv)) {
    return(get("QUEUE_DB_PATH", envir = .GlobalEnv))
  }
  queue_dir <- Sys.getenv("QUEUE_DIR", "api/queue_liteq")
  Sys.getenv("QUEUE_DB_PATH", file.path(queue_dir, "liteq.sqlite"))
}

# Returns pending (READY), running (WORKING) and terminal FAILED counts.
queue_status_counts <- function(queue_db_path = queue_db_path_default(),
                                queue_name = "de_analysis") {
  counts <- list(ready = 0L, working = 0L, failed = 0L)
  if (!file.exists(queue_db_path)) return(counts)

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = queue_db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # liteq takes an exclusive lock while publishing/consuming
  try(DBI::dbExecute(con, "PRAGMA busy_timeout = 2000"), silent = TRUE)

  table <- queue_table_name(queue_name)
  if (!table %in% DBI::dbListTables(con)) return(counts)

  rows <- DBI::dbGetQuery(
    con,
    sprintf("SELECT status, COUNT(*) AS n FROM %s GROUP BY status",
            DBI::dbQuoteIdentifier(con, table))
  )
  for (i in seq_len(nrow(rows))) {
    key <- tolower(as.character(rows$status[i]))
    if (key %in% names(counts)) counts[[key]] <- as.integer(rows$n[i])
  }
  counts
}

# FAILED is terminal in liteq (messages are never retried), so the rows are
# only diagnostics. Successful tasks are acked and deleted, so anything left
# here is either a genuinely failed task or a message from before the worker
# started acking on success.
purge_failed_queue_messages <- function(queue_db_path = queue_db_path_default(),
                                        queue_name = "de_analysis") {
  if (!file.exists(queue_db_path)) return(0L)

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = queue_db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  table <- queue_table_name(queue_name)
  if (!table %in% DBI::dbListTables(con)) return(0L)

  DBI::dbExecute(
    con,
    sprintf("DELETE FROM %s WHERE status = 'FAILED'", DBI::dbQuoteIdentifier(con, table))
  )
}

# Exact state of one analysis's queue message: "queued" while it is READY,
# "running" once a worker has claimed it (WORKING), NULL when the message is
# gone (task finished, failed, or never queued). Keyed by analysis_id inside
# the JSON payload because that works for every task type, and because the
# message row is the only place that knows whether this analysis, rather than
# some other one, is the task a worker is computing right now.
#
# Returns NA_character_ when the queue itself cannot be inspected (locked DB,
# unreadable file). That is deliberately distinct from NULL: "I could not look"
# must not be reported as "this analysis is not queued", because the client
# would then fall back to the stale *_queued status and claim the task is
# waiting when it is in fact running.
queue_task_state <- function(analysis_id, queue_db_path = queue_db_path_default(),
                             queue_name = "de_analysis", task_title = NULL) {
  if (is.null(analysis_id) || length(analysis_id) != 1L ||
        is.na(analysis_id) || !nzchar(analysis_id)) {
    return(NULL)
  }
  if (!file.exists(queue_db_path)) return(NULL)

  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = queue_db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    try(DBI::dbExecute(con, "PRAGMA busy_timeout = 2000"), silent = TRUE)

    table <- queue_table_name(queue_name)
    if (!table %in% DBI::dbListTables(con)) return(NULL)

    sql <- sprintf(
      "SELECT status FROM %s
        WHERE status IN ('READY', 'WORKING') AND message LIKE ?",
      DBI::dbQuoteIdentifier(con, table)
    )
    params <- list(paste0('%"analysis_id":"', analysis_id, '"%'))
    if (!is.null(task_title) && nzchar(task_title)) {
      sql <- paste(sql, "AND title = ?")
      params <- c(params, list(task_title))
    }
    # WORKING first: if a second message for this analysis is waiting while
    # one is being computed, the analysis is running, not queued.
    sql <- paste(sql,
                 "ORDER BY CASE status WHEN 'WORKING' THEN 0 ELSE 1 END, id DESC LIMIT 1")

    rows <- DBI::dbGetQuery(con, sql, params = params)
    if (nrow(rows) == 0) return(NULL)

    switch(as.character(rows$status[1]),
      READY = "queued",
      WORKING = "running",
      NULL
    )
  }, error = function(e) NA_character_)
}

# UUID of a task for this analysis and task type that is already waiting or
# being computed, or NULL.
queue_pending_task_id <- function(analysis_id, task_title,
                                  queue_db_path = queue_db_path_default(),
                                  queue_name = "de_analysis") {
  if (is.null(analysis_id) || !nzchar(analysis_id) ||
        is.null(task_title) || !nzchar(task_title) ||
        !file.exists(queue_db_path)) {
    return(NULL)
  }

  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = queue_db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    try(DBI::dbExecute(con, "PRAGMA busy_timeout = 2000"), silent = TRUE)

    table <- queue_table_name(queue_name)
    if (!table %in% DBI::dbListTables(con)) return(NULL)

    rows <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT message FROM %s
          WHERE status IN ('READY', 'WORKING') AND title = ? AND message LIKE ?
          ORDER BY id DESC",
        DBI::dbQuoteIdentifier(con, table)
      ),
      params = list(task_title, paste0('%"analysis_id":"', analysis_id, '"%'))
    )
    if (nrow(rows) == 0) return(NULL)

    # Newest first, and skip payloads without an id (only tasks published by
    # build_task_payload carry one).
    for (raw in rows$message) {
      payload <- tryCatch(jsonlite::fromJSON(raw), error = function(e) NULL)
      if (!is.null(payload$id)) return(as.character(payload$id))
    }
    NULL
  }, error = function(e) NULL)
}
