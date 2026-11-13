#!/usr/bin/env Rscript

if (!requireNamespace("RestRserve", quietly = TRUE)) {
  cat("SKIP: RestRserve not installed\n")
  quit(status = 0)
}

library(RestRserve)
source("api/helpers/error_handling.R")
source("api/helpers/encrypted_fst.R")

CORS_ALLOWED_HEADERS <- "Content-Type, X-Analysis-Key"
is_demo_analysis <- function(analysis_id) FALSE
validate_analysis_id <- function(analysis_id) invisible(TRUE)

config_path <- tempfile(fileext = ".json")
jsonlite::write_json(list(name_column = "gene_id"), config_path)
get_config_file_path <- function(analysis_id) config_path

app <- Application$new()
source("api/routes/analyses.R")

path <- "/v1/analyses/00000000-0000-0000-0000-000000000001/config"
unauthorized <- app$process_request(Request$new(path = path, method = "GET"))
stopifnot(identical(as.integer(unauthorized$status_code), 403L))

authorized <- app$process_request(Request$new(
  path = path,
  method = "GET",
  headers = list("X-Analysis-Key" = paste(rep("a", 64L), collapse = ""))
))
stopifnot(identical(as.integer(authorized$status_code), 200L))

unlink(config_path)
cat("PASS: private config route requires a valid analysis key\n")
