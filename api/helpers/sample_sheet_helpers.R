# Sample-ID column: well-known names first, else the first column. Shared by
# the upload path and the stored-sheet metadata GET.
detect_sample_id_column <- function(sample_sheet_data) {
  possible_id_cols <- c(
    "sample", "sample_id", "sampleid", "Sample", "Sample_ID", "sampleID",
    "id", "ID", "SampleID", "sample_name", "SampleName", "sample.name"
  )
  for (col in possible_id_cols) {
    if (col %in% names(sample_sheet_data)) {
      return(col)
    }
  }
  names(sample_sheet_data)[1]
}

sample_sheet_metadata <- function(sample_sheet_data, sample_id_col, sample_sheet_hash) {
  column_names <- names(sample_sheet_data)
  column_levels <- list()
  for (col in column_names) {
    vals <- unique(sample_sheet_data[[col]])
    vals <- vals[!is.na(vals)]
    column_levels[[col]] <- I(as.character(vals))
  }

  sample_values <- list()
  for (i in seq_len(nrow(sample_sheet_data))) {
    sid <- as.character(sample_sheet_data[[sample_id_col]][i])
    vals <- lapply(sample_sheet_data[i, , drop = FALSE], function(x) {
      v <- as.character(x[[1]])
      if (is.na(v)) "" else v
    })
    sample_values[[sid]] <- vals
  }

  list(
    status = "complete",
    rows = nrow(sample_sheet_data),
    columns = ncol(sample_sheet_data),
    column_names = as.list(column_names),
    column_levels = column_levels,
    sample_id_column = sample_id_col,
    sample_values = sample_values,
    sample_sheet_hash = sample_sheet_hash
  )
}

process_sample_sheet <- function(sample_sheet_data, analysis_id, key = NULL) {
  if (is.null(sample_sheet_data) || nrow(sample_sheet_data) == 0) {
    raise(error_bad_request("Sample sheet is empty", "The uploaded sample sheet contains no data."))
  }

  sample_id_col <- detect_sample_id_column(sample_sheet_data)

  sample_ids <- as.character(sample_sheet_data[[sample_id_col]])

  if (any(duplicated(sample_ids))) {
    dupes <- unique(sample_ids[duplicated(sample_ids)])
    raise(error_bad_request(
      "Duplicate sample IDs in the sample sheet",
      paste("Duplicate sample IDs:", paste(dupes, collapse = ", ")),
      meta = list(
        validation = "duplicate_sample_ids",
        duplicate_ids = as.list(dupes)
      )
    ))
  }

  counts_file_path <- get_counts_file_path(analysis_id)
  expected_samples <- character(0)
  if (file.exists(counts_file_path)) {
    count_matrix_df <- get_count_matrix_df(counts_file_path, key)
    expected_samples <- names(count_matrix_df)[-1]
  }

  if (length(expected_samples) > 0) {
    missing_samples <- setdiff(expected_samples, sample_ids)
    if (length(missing_samples) > 0) {
      raise(error_bad_request(
        "Samples missing from the sample sheet",
        paste("The following samples from the count data are missing from the sample sheet:", paste(missing_samples, collapse = ", ")),
        meta = list(
          validation = "missing_samples",
          missing_samples = as.list(missing_samples)
        )
      ))
    }

  }

  sample_sheet_path <- file.path(SESSION_DATA_DIR, analysis_id, "sample_sheet.fst")
  tryCatch(
    {
      write_session_fst(sample_sheet_data, sample_sheet_path, key, compress = 30L)
    },
    error = function(e) {
      raise(error_internal_error("Internal server error", "Failed to save sample sheet."))
    }
  )

  sample_sheet_hash <- hash_file(sample_sheet_path)

  sample_sheet_metadata(sample_sheet_data, sample_id_col, sample_sheet_hash)
}

buildColdataFromSampleSheet <- function(sampleSheet, sampleIdColumn, selectedSamples,
                                        condition, covariates = NULL, blockingColumn = NULL) {
  if (length(selectedSamples) == 0) {
    stop("No samples selected for coldata construction")
  }

  if (length(condition) != length(selectedSamples)) {
    stop(sprintf(
      "Condition length (%d) doesn't match selected samples count (%d)",
      length(condition), length(selectedSamples)
    ))
  }

  coldata <- sampleSheet[sampleSheet[[sampleIdColumn]] %in% selectedSamples, , drop = FALSE]

  if (nrow(coldata) == 0) {
    stop("No samples found in sample sheet matching selected samples")
  }

  rownames(coldata) <- coldata[[sampleIdColumn]]

  coldata <- coldata[match(selectedSamples, rownames(coldata)), , drop = FALSE]

  coldata$condition <- factor(condition)

  all_design_vars <- unique(c(covariates, blockingColumn))
  all_design_vars <- all_design_vars[all_design_vars != "" & !is.null(all_design_vars)]

  if (length(all_design_vars) > 0) {
    for (var in all_design_vars) {
      if (var %in% names(coldata)) {
        coldata[[var]] <- factor(coldata[[var]])
      }
    }
  }

  coldata
}

buildDESeq2Design <- function(covariates = NULL, blockingColumn = NULL) {
  terms <- c()

  if (!is.null(blockingColumn) && blockingColumn != "") {
    terms <- c(terms, blockingColumn)
  }

  if (!is.null(covariates) && length(covariates) > 0) {
    terms <- c(terms, covariates)
  }

  terms <- unique(terms)

  terms <- c(terms, "condition")

  as.formula(paste0("~ ", paste(terms, collapse = " + ")))
}
