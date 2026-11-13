MAX_ROWS <- 2e6
MAX_COLS <- 20

create_content_type_middleware <- function() {
  middleware <- EncodeDecodeMiddleware$new()

  middleware$ContentHandlers$set_encode("application/json", function(x, unbox = TRUE) {
    res <- jsonlite::toJSON(x, dataframe = "columns", auto_unbox = unbox, null = "null", na = "null", digits = NA)
    unclass(res)
  })

  middleware$ContentHandlers$set_encode(
    "application/x-ndjson",
    function(x, ...) {
      # If x is already raw, return it; if character vector (lines), join or convert to raw.
      if (is.raw(x)) return(x)
      if (is.character(x)) return(charToRaw(enc2utf8(paste(x, collapse = "\n"))))
      stop("Unsupported body type for NDJSON")
    }
  )

  middleware$ContentHandlers$set_decode("application/json", function(x) {
    if (length(x) == 0) {
      return(NULL)
    }
    tryCatch({
      # Convert raw to char and parse
      jsonlite::fromJSON(rawToChar(x), simplifyVector = TRUE)
    }, error = function(e) {
      # If parsing fails, raise a 400 error immediately
      # This prevents the request from reaching the endpoint
      raise(HTTPError$bad_request(body = paste("Invalid JSON:", conditionMessage(e))))
    })
  })

  # Support text/csv
  middleware$ContentHandlers$set_decode("text/csv", function(x) {
    res <- tryCatch({
      con <- textConnection(rawToChar(x), open = "r")
      on.exit(close(con))
      read.csv(con)
    }, error = function(e) {
      raise(error_bad_request("Invalid CSV", e$message))
    })
    if (nrow(res) > MAX_ROWS) raise(error_bad_request("File has too many rows."))
    if (ncol(res) > MAX_COLS) raise(error_bad_request("File has too many columns."))
    res
  })

  # Support text/tab-separated-values
  middleware$ContentHandlers$set_decode("text/tab-separated-values", function(x) {
    res <- tryCatch({
      con <- textConnection(rawToChar(x), open = "r")
      on.exit(close(con))
      read.delim(con)
    }, error = function(e) {
      raise(error_bad_request("Invalid TSV", e$message))
    })
    if (nrow(res) > MAX_ROWS) raise(error_bad_request("File has too many rows."))
    if (ncol(res) > MAX_COLS) raise(error_bad_request("File has too many columns."))
    res
  })

  # Support application/vnd.openxmlformats-officedocument.spreadsheetml.sheet (XLSX)
  # readxl handles both .xls and .xlsx format detection from file header
  excel_decode <- function(x) {
    res <- tryCatch({
      tmp <- tempfile()
      on.exit(unlink(tmp), add = TRUE)
      body_raw <- NULL
      if (is.raw(x)) {
        body_raw <- x
      } else if (is.character(x)) {
        body_raw <- charToRaw(paste(x, collapse = "\n"))
      } else if (is.list(x)) {
        parts <- lapply(x, function(el) {
          if (is.raw(el)) el else if (is.character(el)) charToRaw(paste(el, collapse="\n")) else charToRaw(as.character(el))
        })
        parts <- parts[lengths(parts) > 0]
        body_raw <- if (length(parts) > 0) do.call(c, parts) else raw()
      } else {
        raise(error_bad_request("Invalid Excel file", paste0("Unexpected body type (", typeof(x), "/", class(x)[1], ") in Excel upload.")))
      }
      con <- file(tmp, "wb")
      base::writeBin(as.integer(body_raw), con, size = 1L)
      close(con)
      readxl::read_excel(tmp)
    }, error = function(e) {
      raise(error_bad_request("Invalid Excel file", e$message))
    })
    if (nrow(res) > MAX_ROWS) raise(error_bad_request("File has too many rows."))
    if (ncol(res) > MAX_COLS) raise(error_bad_request("File has too many columns."))
    res
  }

  middleware$ContentHandlers$set_decode(
    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    excel_decode
  )

  # Support application/vnd.ms-excel (legacy .xls binary format)
  middleware$ContentHandlers$set_decode(
    "application/vnd.ms-excel",
    excel_decode
  )

  middleware
}

stream_ndjson_fst <- function(fst_path, response,
                              key = NULL,
                              cols = NULL,
                              chunk_size = 10000,
                              row_to_json_fn = NULL,
                              sci_notation_cols = c("pvalue", "padj"),
                              sci_notation_threshold = 0.0001,
                              digits = 4,
                              prelude_lines = NULL) {
  if (!is.null(key)) {
    return(with_plain_session_fst(fst_path, key, function(plain_path) {
      stream_ndjson_fst(
        plain_path, response, key = NULL, cols = cols, chunk_size = chunk_size,
        row_to_json_fn = row_to_json_fn, sci_notation_cols = sci_notation_cols,
        sci_notation_threshold = sci_notation_threshold, digits = digits,
        prelude_lines = prelude_lines
      )
    }))
  }
  if (!file.exists(fst_path)) stop("fst file not found: ", fst_path)

  info <- fst::metadata_fst(fst_path)
  nrows <- info$nrOfRows

  if (is.null(row_to_json_fn)) {
    row_to_json_fn <- function(row) jsonlite::toJSON(row, auto_unbox = TRUE, digits = digits, null = "null", na = "null")
  }

  rc <- rawConnection(raw(0), "r+")
  on.exit(close(rc), add = TRUE)

  # Optional header line(s) written before the data rows (e.g. explore's
  # {"samples": [...]} header). Emitted even when there are zero data rows.
  if (!is.null(prelude_lines) && length(prelude_lines) > 0) {
    writeBin(charToRaw(paste0(paste(prelude_lines, collapse = "\n"), "\n")), rc)
  }

  start <- 1L
  while (start <= nrows) {
    end <- min(start + chunk_size - 1L, nrows)
    chunk <- fst::read_fst(fst_path, from = start, to = end, columns = cols, as.data.table = FALSE)
    if (is.null(chunk) || nrow(chunk) == 0) break

    # Convert small values to scientific notation, keep larger values as decimals
    if (!is.null(sci_notation_cols)) {
      for (col in sci_notation_cols) {
        if (col %in% names(chunk) && is.numeric(chunk[[col]])) {
          # Create character vector to hold formatted values
          formatted <- character(length(chunk[[col]]))
          mask <- chunk[[col]] < sci_notation_threshold & !is.na(chunk[[col]])

          # Small values: scientific notation
          formatted[mask] <- format(chunk[[col]][mask], scientific = TRUE, trim = TRUE)

          # Large values: rounded decimals (exclude NAs which remain as NA)
          large_mask <- !mask & !is.na(chunk[[col]])
          formatted[large_mask] <- as.character(round(chunk[[col]][large_mask], digits = 4))

          # Ensure NA values remain as NA (not empty strings)
          formatted[is.na(chunk[[col]])] <- NA_character_

          # Replace column with formatted character vector
          chunk[[col]] <- formatted
        }
      }
    }

    # Convert each row to JSON object string
    json_lines <- vapply(seq_len(nrow(chunk)), function(i) {
      as.character(row_to_json_fn(as.list(chunk[i, , drop = FALSE])))
    }, FUN.VALUE = character(1), USE.NAMES = FALSE)

    block <- paste0(paste(json_lines, collapse = "\n"), "\n")
    writeBin(charToRaw(block), rc)
    start <- end + 1L
  }

  # Send the accumulated raw buffer as the response body
  # not real streaming, since we send everything in 1 batch
  # but to keep the code prepared
  body_raw <- rawConnectionValue(rc)
  response$set_body(body_raw)
  response$set_content_type("application/x-ndjson; charset=utf-8")

  invisible(TRUE)
}
