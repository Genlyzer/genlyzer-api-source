# Genlyzer API Server
# Main REST API server using RestRserve
# Tasks are submitted to a liteq queue; worker monitors process them

library(RestRserve)
library(jsonlite)
library(uuid)
library(dplyr)
library(fst)
library(liteq)
library(data.table)

message("Starting Genlyzer API Server...")

API_VERSION <- "v1"
API_PORT <- as.integer(Sys.getenv("API_PORT", "8080"))
SESSION_DATA_DIR <- Sys.getenv("SESSION_DATA_DIR", "api/session_data")
QUEUE_DIR <- "api/queue_liteq"
QUEUE_DB_PATH <- file.path(QUEUE_DIR, "liteq.sqlite")
CLEANUP_AGE_DAYS <- 7

source("api/helpers/analysis_state.R")
source("api/tasks/queue.R")
source("api/helpers/validation.R")
source("api/helpers/error_handling.R")
source("api/helpers/file_handling.R")
source("api/helpers/sample_sheet_helpers.R")
source("api/helpers/content_types.R")
source("api/helpers/encrypted_fst.R")
source("api/helpers/worker_key_channel.R")
message(sprintf(
  "Worker key channel target: tcp://%s:%d",
  WORKER_KEY_HOST, WORKER_KEY_PORT
))
source("api/helpers/cors.R")
source("api/helpers/versions.R")
source("api/helpers/build_info.R")
source("api/helpers/results_cache.R")
source("api/helpers/queue_stats.R")
source("api/helpers/demo.R")
source("api/helpers/ora_helpers.R")
source("api/helpers/vst_helpers.R")

initialize_session_tmp_dir()

if (!dir.exists(SESSION_DATA_DIR)) {
  dir.create(SESSION_DATA_DIR, recursive = TRUE)
}
if (!dir.exists(QUEUE_DIR)) {
  dir.create(QUEUE_DIR, recursive = TRUE, showWarnings = FALSE)
}
# Note: Queue initialization happens in worker monitor via liteq::ensure_queue()

app <- RestRserve::Application$new(middleware = list(
  create_cors_middleware(),
  create_content_type_middleware()
))
message("RestRserve application initialized")

source("api/routes/analyses.R")
message("Routes for analyses loaded")

yaml_file = file.path("openapi.yaml")
app$add_openapi(path = "/openapi.yaml", file_path = yaml_file)
app$add_swagger_ui(path = "/doc", path_openapi = "/openapi.yaml", use_cdn = TRUE)
message("OpenAPI spec and Swagger UI routes added")

app$add_get(
  path = "/v1/queue/stats",
  FUN = function(request, response) {
    counts <- tryCatch(
      queue_status_counts(QUEUE_DB_PATH),
      error = function(e) {
        message("Failed to read queue stats")
        list(ready = NA_integer_, working = NA_integer_, failed = NA_integer_)
      }
    )

    total <- if (is.na(counts$ready) || is.na(counts$working)) {
      NA_integer_
    } else {
      counts$ready + counts$working
    }

    success_response(response, list(
      timestamp = Sys.time(),
      pending = counts$ready,
      running = counts$working,
      failed = counts$failed,
      queue_length = total
    ))
  }
)

app$add_get(
  path = "/v1/versions",
  FUN = function(request, response) {
    versions <- tryCatch(get_versions_all(), error = function(e) {
      message("Failed to get versions")
      list(mus_musculus = character(), homo_sapiens = character())
    })
    success_response(response, versions)
  }
)

app$add_get(
  path = "/v1/build-info",
  FUN = function(request, response) {
    success_response(response, get_build_info())
  }
)

cat(sprintf("Genlyzer API Server running on port %d (API: /%s)\n", API_PORT, API_VERSION))
if (!interactive()) {
  backend <- BackendRserve$new()
  on.exit({
    try(Rserve::shutdown(), silent = TRUE)
  }, add = TRUE)
  backend$start(app, http_port = API_PORT, background = FALSE)
}
