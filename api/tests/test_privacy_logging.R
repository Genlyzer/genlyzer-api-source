#!/usr/bin/env Rscript

if (!requireNamespace("RestRserve", quietly = TRUE)) {
  cat("SKIP: RestRserve not installed\n")
  quit(status = 0)
}

library(RestRserve)
source("api/helpers/error_handling.R")

sentinel <- "SENSITIVE_USER_VALUE_SENTINEL"
error_output <- capture.output(
  invisible(error_bad_request("Invalid sample column", paste("Column:", sentinel)))
)
response_output <- capture.output(
  invisible(success_response(Response$new(), list(sample_name = sentinel)))
)

stopifnot(
  !any(grepl(sentinel, error_output, fixed = TRUE)),
  !any(grepl(sentinel, response_output, fixed = TRUE)),
  length(error_output) == 0L,
  length(response_output) == 0L
)

cat("PASS: API error and success helpers do not log response details\n")
