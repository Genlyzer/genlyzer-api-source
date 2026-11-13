WORKER_KEY_PORT <- as.integer(Sys.getenv("WORKER_KEY_PORT", "55556"))
WORKER_KEY_HOST <- "127.0.0.1"
WORKER_KEY_MAX_AGE_SECONDS <- 3600

publish_worker_analysis_key <- function(analysis_id, key) {
  if (!is.character(analysis_id) || length(analysis_id) != 1L ||
        !grepl("^[A-Za-z0-9-]+$", analysis_id) ||
        !is.character(key) || length(key) != 1L ||
        !grepl("^[0-9a-fA-F]{64}$", key)) {
    stop("Invalid worker key handoff", call. = FALSE)
  }

  payload <- charToRaw(jsonlite::toJSON(
    list(analysis_id = analysis_id, key = tolower(key)),
    auto_unbox = TRUE
  ))
  helper <- "api/tasks/publish_worker_key.R"
  rscript <- file.path(R.home("bin"), "Rscript")
  ensure_session_tmp_dir()
  payload_path <- tempfile(
    pattern = "genlyzer-key-",
    tmpdir = SESSION_DECRYPTED_TMP_DIR
  )
  on.exit(unlink(payload_path, force = TRUE), add = TRUE)
  writeBin(payload, payload_path)
  Sys.chmod(payload_path, mode = "0600")

  # RestRserve forks request handlers; run NNG in a fresh process rather than
  # using its native context from a forked handler.
  output <- suppressWarnings(system2(
    rscript,
    args = shQuote(c(helper, payload_path)),
    stdout = TRUE,
    stderr = TRUE,
    env = paste0("WORKER_KEY_PORT=", WORKER_KEY_PORT)
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (!identical(as.integer(status), 0L) ||
        !identical(tail(output, 1L), "OK")) {
    details <- paste(output, collapse = "\n")
    stop(sprintf(
      "Analysis worker key handoff subprocess failed at %s:%d%s",
      WORKER_KEY_HOST, WORKER_KEY_PORT,
      if (nzchar(details)) paste0(": ", details) else ""
    ), call. = FALSE)
  }
  invisible(TRUE)
}

# Publish an analysis key
handoff_worker_analysis_key <- function(analysis_id, key) {
  tryCatch(
    publish_worker_analysis_key(analysis_id, key),
    error = function(e) {
      message("Worker key handoff failed")
      raise(error_service_unavailable(
        "Calculation workers are unavailable",
        paste(
          sprintf(
            "The API could not hand the analysis key to the worker at %s:%d.",
            WORKER_KEY_HOST, WORKER_KEY_PORT
          ),
          "Confirm the worker monitor is running and that the API and worker",
          "use the same WORKER_KEY_PORT, then retry."
        )
      ))
    }
  )
}

new_worker_key_registry <- function() {
  new.env(parent = emptyenv())
}

new_worker_key_server <- function(port = WORKER_KEY_PORT) {
  if (length(port) != 1L || is.na(port) || port < 1L || port > 65535L) {
    stop("Invalid worker key channel port", call. = FALSE)
  }
  nanonext::socket(
    "rep",
    listen = sprintf("tcp://%s:%d", WORKER_KEY_HOST, as.integer(port))
  )
}

poll_worker_key_handoffs <- function(server, registry) {
  if (is.null(server)) return(invisible(FALSE))

  payload <- nanonext::recv(server, mode = "raw", block = FALSE)
  if (nanonext::is_error_value(payload)) {
    if (identical(as.integer(payload), 8L)) return(invisible(FALSE))
    stop(
      "Worker key channel receive failed: ",
      nanonext::nng_error(as.integer(payload)),
      call. = FALSE
    )
  }

  response <- "ERROR"
  request <- tryCatch({
    if (length(payload) == 0L || length(payload) > 256L) {
      stop("Invalid key handoff message")
    }
    jsonlite::fromJSON(rawToChar(payload))
  }, error = function(e) NULL)

  valid_request <- !is.null(request) &&
    is.list(request) &&
    identical(sort(names(request)), c("analysis_id", "key")) &&
    is.character(request$analysis_id) &&
    length(request$analysis_id) == 1L &&
    grepl("^[A-Za-z0-9-]+$", request$analysis_id) &&
    is.character(request$key) &&
    length(request$key) == 1L &&
    grepl("^[0-9a-fA-F]{64}$", request$key)

  if (valid_request) {
    assign(
      request$analysis_id,
      list(key = tolower(request$key), received_at = Sys.time()),
      envir = registry
    )
    response <- "OK"
  }

  sent <- nanonext::send(server, charToRaw(response), mode = "raw", block = 5000)
  if (nanonext::is_error_value(sent) || !identical(as.integer(sent), 0L)) {
    stop("Worker key channel reply failed", call. = FALSE)
  }
  invisible(identical(response, "OK"))
}

take_worker_analysis_key <- function(analysis_id, registry) {
  if (!exists(analysis_id, envir = registry, inherits = FALSE)) {
    stop("Encryption key unavailable; resubmit this analysis", call. = FALSE)
  }
  entry <- get(analysis_id, envir = registry, inherits = FALSE)
  rm(list = analysis_id, envir = registry)
  age <- as.numeric(difftime(Sys.time(), entry$received_at, units = "secs"))
  if (!is.finite(age) || age > WORKER_KEY_MAX_AGE_SECONDS) {
    stop("Encryption key expired; resubmit this analysis", call. = FALSE)
  }
  analysis_key_from_header(entry$key)
}
