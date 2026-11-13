# Logging Helper
# Provides logging functionality for both API server and mirai workers

get_log_path <- function() {
  log_dir <- Sys.getenv("LOG_DIR", "api/logs")
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  }
  file.path(log_dir, "genlyzer.log")
}

# Log a message to file (works in mirai workers)
log_to_file <- function(level, msg, analysis_id = NULL) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%OS3")
  prefix <- if (!is.null(analysis_id)) {
    sprintf("[%s] [%s] %s", timestamp, level, analysis_id)
  } else {
    sprintf("[%s] [%s]", timestamp, level)
  }
  log_line <- sprintf("%s %s", prefix, msg)

  log_file <- get_log_path()
  cat(log_line, "\n", file = log_file, append = TRUE)
}

log_info <- function(msg, analysis_id = NULL) {
  log_to_file("INFO", msg, analysis_id)
}

log_warn <- function(msg, analysis_id = NULL) {
  log_to_file("WARN", msg, analysis_id)
}

log_error <- function(msg, analysis_id = NULL) {
  log_to_file("ERROR", msg, analysis_id)
}

log_debug <- function(msg, analysis_id = NULL) {
  log_to_file("DEBUG", msg, analysis_id)
}
