#!/usr/bin/env Rscript

WORKER_KEY_PORT <- as.integer(Sys.getenv("WORKER_KEY_PORT", "55556"))
WORKER_KEY_HOST <- "127.0.0.1"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !file.exists(args[[1L]])) {
  stop("Missing worker key handoff payload file", call. = FALSE)
}

payload_size <- file.info(args[[1L]])$size
if (is.na(payload_size) || payload_size < 1L || payload_size > 256L) {
  stop("Invalid worker key handoff payload size", call. = FALSE)
}
payload <- readBin(args[[1L]], what = "raw", n = payload_size)
request <- jsonlite::fromJSON(rawToChar(payload))
if (!is.list(request) ||
      !identical(sort(names(request)), c("analysis_id", "key")) ||
      !is.character(request$analysis_id) || length(request$analysis_id) != 1L ||
      !grepl("^[A-Za-z0-9-]+$", request$analysis_id) ||
      !is.character(request$key) || length(request$key) != 1L ||
      !grepl("^[0-9a-fA-F]{64}$", request$key)) {
  stop("Invalid worker key handoff payload", call. = FALSE)
}

socket <- nanonext::socket(
  "req",
  dial = sprintf("tcp://%s:%d", WORKER_KEY_HOST, WORKER_KEY_PORT)
)
on.exit(close(socket), add = TRUE)

sent <- nanonext::send(socket, payload, mode = "raw", block = 5000)
if (nanonext::is_error_value(sent) || !identical(as.integer(sent), 0L)) {
  status <- if (nanonext::is_error_value(sent)) {
    paste(as.integer(sent), nanonext::nng_error(as.integer(sent)))
  } else {
    as.character(sent)
  }
  stop(sprintf("Worker key channel send failed (%s)", status), call. = FALSE)
}

response <- nanonext::recv(socket, mode = "character", block = 30000)
if (nanonext::is_error_value(response)) {
  stop(
    "Worker key channel response failed: ",
    nanonext::nng_error(as.integer(response)),
    call. = FALSE
  )
}
if (!identical(response, "OK")) {
  stop("Worker rejected key handoff", call. = FALSE)
}

cat("OK\n")
