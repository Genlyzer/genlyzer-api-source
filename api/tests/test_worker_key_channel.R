#!/usr/bin/env Rscript

if (!requireNamespace("nanonext", quietly = TRUE) ||
    !requireNamespace("jsonlite", quietly = TRUE) ||
    !requireNamespace("RestRserve", quietly = TRUE)) {
  stop("Worker key channel tests require nanonext, jsonlite, and RestRserve",
       call. = FALSE)
}

library(RestRserve)

source("api/helpers/encrypted_fst.R")
source("api/helpers/error_handling.R")
source("api/helpers/worker_key_channel.R")

run_worker_key_channel_tests <- function() {
  test_tmp_dir <- tempfile("worker-key-test-")
  dir.create(test_tmp_dir, mode = "0700")
  previous_tmp_dir <- SESSION_DECRYPTED_TMP_DIR
  SESSION_DECRYPTED_TMP_DIR <<- test_tmp_dir
  on.exit({
    SESSION_DECRYPTED_TMP_DIR <<- previous_tmp_dir
    unlink(test_tmp_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  integration_port <- 42000L + sample.int(10000L, 1L)
  integration_key <- paste(sprintf("%02x", 255L:224L), collapse = "")
  WORKER_KEY_PORT <<- integration_port
  receiver_script <- normalizePath("api/tests/worker_key_receiver.R")
  integration_rscript <- file.path(R.home("bin"), "Rscript")
  receiver_log <- tempfile("worker-key-receiver-")
  on.exit(unlink(receiver_log), add = TRUE)
  receiver_pid <- system2(
    integration_rscript,
    args = shQuote(receiver_script),
    stdout = receiver_log,
    stderr = FALSE,
    wait = FALSE,
    env = c(
      paste0("WORKER_KEY_PORT=", integration_port),
      paste0("WORKER_KEY_TEST_KEY=", integration_key)
    )
  )
  on.exit(tools::pskill(receiver_pid), add = TRUE)

  deadline <- Sys.time() + 10
  repeat {
    receiver_output <- if (file.exists(receiver_log)) {
      readLines(receiver_log, warn = FALSE)
    } else {
      character()
    }
    if ("READY" %in% receiver_output) break
    if (Sys.time() >= deadline) {
      stop("Timed out waiting for subprocess test receiver")
    }
    Sys.sleep(0.01)
  }

  publish_worker_analysis_key("subprocess-test", integration_key)
  stopifnot(length(list.files(
    test_tmp_dir, pattern = "^genlyzer-key-"
  )) == 0L)
  deadline <- Sys.time() + 10
  repeat {
    receiver_output <- readLines(receiver_log, warn = FALSE)
    if ("KEY_OK" %in% receiver_output) break
    if (Sys.time() >= deadline) {
      stop("Subprocess test receiver did not confirm the key handoff")
    }
    Sys.sleep(0.01)
  }
  unlink(receiver_log)
  cat("PASS: API key handoff subprocess communicates with the worker receiver\n")

  port <- 42000L + sample.int(10000L, 1L)
  server <- new_worker_key_server(port)
  on.exit(close(server), add = TRUE)
  registry <- new_worker_key_registry()

  stopifnot(identical(poll_worker_key_handoffs(server, registry), FALSE))

  client <- nanonext::socket(
    "req",
    dial = sprintf("tcp://%s:%d", WORKER_KEY_HOST, port)
  )
  on.exit(close(client), add = TRUE)

  send_payload <- function(payload) {
    sent <- nanonext::send(client, charToRaw(payload), mode = "raw", block = 5000)
    if (nanonext::is_error_value(sent) || !identical(as.integer(sent), 0L)) {
      stop("Could not send test handoff")
    }
  }

  key_hex <- paste(sprintf("%02x", seq_len(32L)), collapse = "")
  valid_payload <- jsonlite::toJSON(
    list(analysis_id = "analysis-test", key = key_hex),
    auto_unbox = TRUE
  )
  send_payload(valid_payload)
  deadline <- Sys.time() + 5
  received <- FALSE
  repeat {
    received <- poll_worker_key_handoffs(server, registry)
    if (isTRUE(received)) break
    if (Sys.time() >= deadline) stop("Timed out waiting for key handoff")
    Sys.sleep(0.01)
  }
  stopifnot(identical(nanonext::recv(client, mode = "character", block = 5000), "OK"))
  stopifnot(identical(
    take_worker_analysis_key("analysis-test", registry),
    analysis_key_from_header(key_hex)
  ))
  stopifnot(inherits(
    try(take_worker_analysis_key("analysis-test", registry), silent = TRUE),
    "try-error"
  ))

  # An unreachable key channel must surface as a CORS-enabled 503 with an
  # actionable message, not a bare 500 that the browser reports as a network
  # failure.
  free_port <- local({
    for (i in seq_len(50L)) {
      candidate <- 42000L + sample.int(10000L, 1L)
      probe <- tryCatch(new_worker_key_server(candidate), error = function(e) NULL)
      if (!is.null(probe)) {
        close(probe)
        return(candidate)
      }
    }
    stop("Could not find a free port")
  })
  previous_port <- WORKER_KEY_PORT
  WORKER_KEY_PORT <<- free_port
  on.exit(WORKER_KEY_PORT <<- previous_port, add = TRUE)

  handoff_error <- tryCatch(
    handoff_worker_analysis_key("analysis-test", key_hex),
    error = function(e) e
  )
  stopifnot(inherits(handoff_error, "HTTPErrorRaise"))
  stopifnot(identical(handoff_error$response$status_code, 503L))
  stopifnot(identical(
    handoff_error$response$get_header("Access-Control-Allow-Origin"), "*"
  ))
  stopifnot(grepl(
    "WORKER_KEY_PORT", handoff_error$response$body$detail, fixed = TRUE
  ))

  cat("PASS: loopback worker key handoff, empty poll, and one-shot key consumption\n")
  cat("PASS: unavailable key channel raises a CORS-enabled 503 with a fix hint\n")
}

run_worker_key_channel_tests()
