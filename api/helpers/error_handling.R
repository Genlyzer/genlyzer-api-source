# Error Handling Helpers
# Standardized error responses following OpenAPI spec and RFC 9457 Problem Details format
# See: https://tools.ietf.org/html/rfc9457

# Note: RestRserve's HTTPError methods (bad_request, not_found, etc.) accept
# a 'body' argument directly. We raise them using RestRserve::raise().

HTTPError$set_encode(function(body, ...) {
  jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE)
})
HTTPError$set_content_type("application/problem+json")

#' Create RFC 9457 Problem Details error body
#' @param type URI reference identifying the problem type (default: "about:blank")
#' @param title Short, human-readable summary of the problem
#' @param status HTTP status code
#' @param detail Human-readable explanation of the specific error
#' @param instance Optional URI reference for the specific error occurrence
#' @return List formatted as RFC 9457 Problem Details
problem_details <- function(type = "about:blank", title, status, detail = NULL, instance = NULL, meta = NULL) {
  body <- list(
    type = type,
    title = title,
    status = status
  )
  if (!is.null(detail)) body$detail <- detail
  if (!is.null(instance)) body$instance <- instance
  if (!is.null(meta)) body$meta <- meta
  body
}

add_cors_headers <- function(err) {
  err$set_header("Access-Control-Allow-Origin", "*")
  err$set_header("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS, PATCH")
  err$set_header("Access-Control-Allow-Headers", "Content-Type, X-File-Name, Authorization, X-Analysis-Key")
  err$set_header("Access-Control-Max-Age", "86400")
  err
}

# 400 Bad Request error
error_bad_request <- function(title, detail = NULL, meta = NULL) {
  add_cors_headers(HTTPError$bad_request(body = problem_details(
    title = title,
    status = 400,
    detail = detail,
    meta = meta
  )))
}

# 404 Not Found error
error_not_found <- function(title, detail = NULL, meta = NULL) {
  add_cors_headers(HTTPError$not_found(body = problem_details(
    title = title,
    status = 404,
    detail = detail,
    meta = meta
  )))
}

# 409 Conflict error
error_conflict <- function(title, detail = NULL, meta = NULL) {
  add_cors_headers(HTTPError$conflict(body = problem_details(
    title = title,
    status = 409,
    detail = detail,
    meta = meta
  )))
}

# 422 Unprocessable Entity error
error_unprocessable <- function(title, detail = NULL, meta = NULL) {
  add_cors_headers(HTTPError$unprocessable_entity(body = problem_details(
    title = title,
    status = 422,
    detail = detail,
    meta = meta
  )))
}

# 403 Forbidden error
error_forbidden <- function(title, detail = NULL, meta = NULL) {
  add_cors_headers(HTTPError$forbidden(body = problem_details(
    title = title,
    status = 403,
    detail = detail,
    meta = meta
  )))
}

# 410 Gone error (completed results evicted from the shared cache)
error_gone <- function(title, detail = NULL, meta = NULL) {
  add_cors_headers(HTTPError$gone(body = problem_details(
    title = title,
    status = 410,
    detail = detail,
    meta = meta
  )))
}

# 503 Service Unavailable error
error_service_unavailable <- function(title, detail = NULL, meta = NULL) {
  add_cors_headers(HTTPError$service_unavailable(body = problem_details(
    title = title,
    status = 503,
    detail = detail,
    meta = meta
  )))
}

# 500 Internal Server Error
error_internal_error <- function(title = "Internal Server Error", detail = NULL, meta = NULL) {
  add_cors_headers(HTTPError$internal_server_error(body = problem_details(
    title = title,
    status = 500,
    detail = detail,
    meta = meta
  )))
}

# 507 Insufficient Storage error
error_insufficient_storage <- function(title, detail = NULL, meta = NULL) {
  add_cors_headers(HTTPError$insufficient_storage(body = problem_details(
    title = title,
    status = 507,
    detail = detail,
    meta = meta
  )))
}

write_json <- function(obj, file, pretty = FALSE, auto_unbox = TRUE) {
  jsonlite::write_json(obj, file, pretty = pretty, auto_unbox = auto_unbox)
}

read_json <- function(file) {
  jsonlite::read_json(file)
}

success_response <- function(response, data, status_code = 200L) {
  response$set_status_code(status_code)
  response$set_body(data)
  response$set_content_type("application/json")
  response
}
