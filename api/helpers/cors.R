# CORS Helpers

CORS_ALLOWED_HEADERS <- "Content-Type, Authorization, X-File-Name, X-Analysis-Key"

#' Create a CORS middleware that sets fixed CORS headers
#' @return RestRserve Middleware object
create_cors_middleware <- function() {
  Middleware$new(
    id = "CORSMiddleware",
    process_request = function(request, response) TRUE,
    process_response = function(request, response) {
      if (response$status_code < 300) {
        response$set_header("Access-Control-Allow-Origin", "*")

        # Presence of "Access-Control-Request-Method" means this is a CORS preflight
        if (request$method == "OPTIONS" &&
              !is.null(request$get_header("Access-Control-Request-Method"))) {
          allow <- response$get_header("Allow")
          response$delete_header("Allow")
          if (!is.null(allow)) {
            response$set_header("Access-Control-Allow-Methods", allow)
          }
          response$set_header("Access-Control-Allow-Headers", CORS_ALLOWED_HEADERS)
          response$set_header("Access-Control-Max-Age", "86400")
        }
      }
      invisible(TRUE)
    }
  )
}
