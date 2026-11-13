#!/usr/bin/env Rscript

source("api/helpers/encrypted_fst.R")
source("api/helpers/worker_key_channel.R")

expected_key <- Sys.getenv("WORKER_KEY_TEST_KEY")
if (!grepl("^[0-9a-fA-F]{64}$", expected_key)) {
  stop("Invalid test key", call. = FALSE)
}

server <- new_worker_key_server()
on.exit(close(server), add = TRUE)
registry <- new_worker_key_registry()

cat("READY\n")
flush(stdout())

deadline <- Sys.time() + 30
repeat {
  if (isTRUE(poll_worker_key_handoffs(server, registry))) break
  if (Sys.time() >= deadline) stop("Timed out waiting for test handoff")
  Sys.sleep(0.01)
}

received_key <- analysis_key_to_hex(
  take_worker_analysis_key("subprocess-test", registry)
)
if (!identical(tolower(received_key), tolower(expected_key))) {
  stop("Subprocess delivered the wrong test key", call. = FALSE)
}

cat("KEY_OK\n")
