get_counts_file_path <- function(analysis_id) {
  file.path(SESSION_DATA_DIR, analysis_id, "count_matrix.fst")
}

counts_file_exists <- function(analysis_id) {
  file.exists(get_counts_file_path(analysis_id))
}

get_config_file_path <- function(analysis_id) {
  file.path(SESSION_DATA_DIR, analysis_id, "config.json")
}

config_file_exists <- function(analysis_id) {
  file.exists(get_config_file_path(analysis_id))
}

get_sample_sheet_path <- function(analysis_id) {
  file.path(SESSION_DATA_DIR, analysis_id, "sample_sheet.fst")
}

sample_sheet_exists <- function(analysis_id) {
  file.exists(get_sample_sheet_path(analysis_id))
}

load_sample_sheet <- function(analysis_id, key = NULL) {
  path <- get_sample_sheet_path(analysis_id)
  if (!file.exists(path)) {
    return(NULL)
  }
  read_session_fst(path, key)
}

MAX_SAMPLES <- 18

collapse_intervals <- function(x, max_display = 20) {
  if (length(x) == 0) {
    return("")
  }
  x <- sort(unique(as.integer(x)))
  intervals <- list()
  start <- x[1]
  end <- x[1]
  for (i in seq_along(x)[-1]) {
    if (x[i] == end + 1) {
      end <- x[i]
    } else {
      intervals <- c(intervals, list(c(start, end)))
      start <- x[i]
      end <- x[i]
    }
  }
  intervals <- c(intervals, list(c(start, end)))

  parts <- vapply(intervals, function(iv) {
    if (iv[1] == iv[2]) as.character(iv[1]) else paste0(iv[1], "-", iv[2])
  }, character(1))

  if (length(parts) > max_display) {
    parts <- c(parts[1:max_display], "...")
  }

  paste(parts, collapse = ", ")
}

truncate_values <- function(x, max_show = 5) {
  if (length(x) == 0) {
    return("")
  }
  x <- as.character(x)
  if (length(x) <= max_show) {
    return(paste(x, collapse = ", "))
  }
  paste0(paste(x[1:max_show], collapse = ", "), " ... (and ", length(x) - max_show, " more)")
}

validate_gene_ids <- function(gene_column) {
  gene_column <- as.character(gene_column)

  # Check for empty/null values
  empty_mask <- is.na(gene_column) | gene_column == ""
  empty_rows <- which(empty_mask)
  if (length(empty_rows) > 0) {
    raise(error_bad_request(
      "Gene names column contains empty values",
      paste("Row numbers:", collapse_intervals(empty_rows + 1L)),
      meta = list(
        validation = "empty_gene_ids",
        empty_rows = empty_rows + 1L
      )
    ))
  }

  # Check for duplicate gene IDs
  dup_mask <- duplicated(gene_column)
  dup_rows <- which(dup_mask)
  if (length(dup_rows) > 0) {
    dup_values <- unique(gene_column[dup_mask])
    # Build first-occurrence mapping for each duplicated value
    first_occurrence <- vapply(dup_values, function(v) {
      which(gene_column == v)[1]
    }, integer(1))
    pairs <- vapply(dup_rows, function(r) {
      v <- gene_column[r]
      fo <- first_occurrence[v]
      paste0("row ", fo + 1L, " & ", r + 1L, " (", v, ")")
    }, character(1))
    raise(error_bad_request(
      "Duplicate ENSEMBL IDs detected",
      truncate_values(unique(pairs)),
      meta = list(
        validation = "duplicate_gene_ids",
        duplicate_rows = dup_rows + 1L,
        duplicate_values = as.list(dup_values)
      )
    ))
  }

  # Validate Ensembl ID format
  valid_ensembl <- grepl("^ENS[A-Z]+[0-9]{11}$", gene_column)
  invalid_rows <- which(!valid_ensembl)
  if (length(invalid_rows) > 0) {
    invalid_values <- gene_column[invalid_rows]
    raise(error_bad_request(
      "Invalid ENSEMBL IDs detected",
      paste(
        "Expected format: ENSXXXXXXXXXXX (e.g., ENSG00000139618)",
        "| Row numbers:", collapse_intervals(invalid_rows + 1L),
        "| Invalid values:", truncate_values(invalid_values)
      ),
      meta = list(
        validation = "invalid_ensembl_ids",
        invalid_rows = invalid_rows + 1L,
        invalid_values = as.list(invalid_values)
      )
    ))
  }
}

validate_count_columns <- function(counts_data) {
  if (ncol(counts_data) < 2) {
    raise(error_bad_request(
      "No sample columns found",
      "The count data must have at least 2 columns (gene IDs + at least 1 sample)."
    ))
  }

  if (ncol(counts_data) - 1 > MAX_SAMPLES) {
    raise(error_bad_request(
      "Too many sample columns",
      paste0("Maximum ", MAX_SAMPLES, " samples allowed, found ", ncol(counts_data) - 1, "."),
      meta = list(
        validation = "too_many_samples",
        max_samples = MAX_SAMPLES,
        found_samples = ncol(counts_data) - 1
      )
    ))
  }

  sample_cols <- names(counts_data)[-1]

  for (i in seq_along(sample_cols)) {
    col_name <- sample_cols[i]
    col_data <- counts_data[[col_name]]

    if (!is.numeric(col_data)) {
      # Allow coercion from character if all values are valid integers
      if (is.character(col_data)) {
        numeric_col <- suppressWarnings(as.numeric(col_data))
        if (any(is.na(numeric_col) & !is.na(col_data))) {
          non_numeric_rows <- which(is.na(numeric_col) & !is.na(col_data))
          raise(error_bad_request(
            "Non-numeric values in sample column",
            paste0("Column '", col_name, "' contains non-numeric values at rows: ", collapse_intervals(non_numeric_rows + 1L)),
            meta = list(
              validation = "non_numeric_counts",
              sample_column = col_name,
              invalid_rows = non_numeric_rows + 1L
            )
          ))
        }
        col_data <- numeric_col
      } else {
        raise(error_bad_request(
          "Invalid data type in sample column",
          paste0("Column '", col_name, "' has unsupported type: ", typeof(col_data), "."),
          meta = list(
            validation = "invalid_column_type",
            sample_column = col_name,
            column_type = typeof(col_data)
          )
        ))
      }
    }

    invalid_rows <- which(is.na(col_data) | !is.finite(col_data))
    if (length(invalid_rows) > 0) {
      raise(error_bad_request(
        "Missing or non-finite count values",
        paste0("Column '", col_name, "' contains invalid values at rows: ", collapse_intervals(invalid_rows + 1L)),
        meta = list(
          validation = "invalid_count_values",
          sample_column = col_name,
          invalid_rows = invalid_rows + 1L
        )
      ))
    }

    # Check for negative values
    neg_rows <- which(!is.na(col_data) & col_data < 0)
    if (length(neg_rows) > 0) {
      raise(error_bad_request(
        "Negative count values detected",
        paste0("Column '", col_name, "' contains negative values at rows: ", collapse_intervals(neg_rows + 1L)),
        meta = list(
          validation = "negative_counts",
          sample_column = col_name,
          negative_rows = neg_rows + 1L
        )
      ))
    }

    counts_data[[col_name]] <- col_data
  }

  column_sums <- vapply(counts_data[sample_cols], sum, numeric(1))
  normalized_scales <- c(100, 10000, 1000000)
  if (length(column_sums) > 1 &&
        all(column_sums == column_sums[1]) &&
        column_sums[1] %in% normalized_scales) {
    raise(error_bad_request(
      "Normalized expression values detected",
      paste0(
        "All sample columns sum to ", format(column_sums[1], scientific = FALSE),
        ", which is characteristic of normalized values such as TPM or CPM. ",
        "Please upload raw gene counts."
      ),
      meta = list(
        validation = "normalized_counts",
        column_sums = as.list(column_sums),
        detected_scale = unname(column_sums[1])
      )
    ))
  }

  counts_data
}

process_count_data <- function(counts_data, analysis_id, key = NULL) {
  if (!is.data.frame(counts_data) || nrow(counts_data) == 0) {
    raise(error_bad_request("Uploaded counts data is empty or invalid."))
  }

  if (any(duplicated(names(counts_data)))) {
    raise(error_bad_request(
      "Duplicate column names",
      paste("Duplicated names:", paste(names(counts_data)[duplicated(names(counts_data))], collapse = ", ")),
      meta = list(
        validation = "duplicate_column_names",
        duplicated_columns = as.list(names(counts_data)[duplicated(names(counts_data))])
      )
    ))
  }

  # Validate gene IDs in first column
  gene_column <- counts_data[[1]]
  validate_gene_ids(gene_column)

  # Validate count columns (columns 2+)
  counts_data <- validate_count_columns(counts_data)

  counts_for_fst <- as.data.frame(counts_data, stringsAsFactors = FALSE, check.names = FALSE)

  # try to write fst
  tryCatch(
    {
      fst_path <- file.path(SESSION_DATA_DIR, analysis_id, "count_matrix.fst")
      write_session_fst(counts_for_fst, fst_path, key, compress = as.integer(30L))
    },
    error = function(e) {
      raise(error_internal_error("Internal server error", "An error occurred while saving the count data. Please try again."))
    }
  )

  list(
    success = TRUE,
    nrows = nrow(counts_data),
    ncolumns = ncol(counts_data),
    column_names = colnames(counts_data)
  )
}

get_count_matrix_df <- function(counts_file_path, key = NULL) {
  if (!file.exists(counts_file_path)) raise(error_internal_error()) # must exist at this point

  tryCatch(
    {
      count_matrix_df <- read_session_fst(counts_file_path, key)
    },
    error = function(e) {
      raise(error_internal_error())
    }
  )

  count_matrix_df
}

check_disk_space <- function(min_gb = 1) {
  # Use the standard POSIX -k flag (sizes in 1KB blocks)
  df_output <- system2("df", c("-k", SESSION_DATA_DIR), stdout = TRUE)

  # Parse the output line (skip header)
  lines <- df_output[2]
  if (is.na(lines) || length(lines) == 0) {
    return(list(valid = TRUE)) # Assume OK if can't determine
  }

  parts <- strsplit(lines, " +")[[1]]
  parts <- parts[parts != ""]

  # 4th column is available space in 1KB blocks
  available_kb <- as.numeric(parts[4])

  # Convert 1KB blocks to Gigabytes (1 GB = 1,048,576 KB)
  available_gb <- available_kb / 1024 / 1024

  if (available_gb < min_gb) {
    message("Insufficient disk space: ", round(available_gb, 2), " GB available, but at least ", min_gb, " GB required.")
    raise(error_internal_error("Internal server error", "The server is currently experiencing high load and may not be able to process your request. Please try again later."))
  }

  list(valid = TRUE, available_gb = available_gb)
}
