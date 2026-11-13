analysis_exists <- function(analysis_id) {
  file.exists(file.path(SESSION_DATA_DIR, analysis_id))
}

validate_analysis_id <- function(analysis_id, must_exist = TRUE) {
  if (is.null(analysis_id) || analysis_id == "") {
    raise(error_bad_request("Missing analysis_id in request"))
  }

  if (!is.character(analysis_id)) {
    raise(error_bad_request("analysis_id must be a string"))
  }

  if (must_exist && !analysis_exists(analysis_id)) {
    raise(error_not_found("Analysis not found"))
  }

  list(valid = TRUE)
}

validate_config <- function(config, analysis_id, key = NULL) {
  if (is.null(config)) raise(error_bad_request("Missing configuration in request body"))

  # Validating parameters according to openapi spec (CountsConfig schema)
  validConfig <- config

  if (is.null(config$organism)) {
    validConfig$organism <- "Mus musculus"
  } else {
    valid_organisms <- c("Mus musculus", "Homo sapiens")
    if (!(config$organism %in% valid_organisms)) {
      raise(error_bad_request("Invalid organism", "Must be 'Mus musculus' or 'Homo sapiens'"))
    }
  }

  required_fields <- c("name_column", "sample_columns")
  missing_fields <- setdiff(required_fields, names(config))
  if (length(missing_fields) > 0) {
    raise(error_bad_request("Missing required fields", paste(missing_fields, collapse = ", ")))
  }

  if (!counts_file_exists(analysis_id)) {
    raise(error_unprocessable("Cannot configure analysis without uploaded count data. Please upload count data first."))
  }
  counts_file_path <- get_counts_file_path(analysis_id)
  count_matrix_df <- get_count_matrix_df(counts_file_path, key)

  # make sure the selected organism conforms to the counts name column ensembl types
  if (config$organism == "Mus musculus") {
    if (!all(grepl("^ENSMUSG", count_matrix_df[[config$name_column]]))) {
      raise(error_bad_request("Invalid name_column", "For the selected organism, name_column must contain Mus musculus Ensembl gene IDs"))
    }
  } else if (config$organism == "Homo sapiens") {
    if (!all(grepl("^ENSG", count_matrix_df[[config$name_column]]))) {
      raise(error_bad_request("Invalid name_column", "For the selected organism, name_column must contain Homo sapiens Ensembl gene IDs"))
    }
  }

  if (!(config$name_column %in% names(count_matrix_df))) {
    raise(error_bad_request("Invalid name_column", paste("name_column must be one of:", paste(names(count_matrix_df), collapse = ", "))))
  }

  if (length(config$sample_columns) == 0) {
    raise(error_bad_request("Invalid sample_columns", "At least one sample column must be provided"))
  }

  if (!all(config$sample_columns %in% names(count_matrix_df))) {
    raise(error_bad_request("Invalid sample_columns", paste("sample_columns must be a subset of:", paste(names(count_matrix_df), collapse = ", "))))
  }

  de_parameters <- config$de_parameters
  if (!is.null(de_parameters)) {
    if (is.null(de_parameters$model)) {
      de_parameters$model <- "DESeq2"
    } else {
      valid_models <- c("DESeq2", "DESeq2 (Rust)", "Limma-Voom")
      if (!(de_parameters$model %in% valid_models)) {
        raise(error_bad_request("Invalid model", "Must be one of: DESeq2, DESeq2 (Rust), Limma-Voom"))
      }
    }

    reference_samples <- unlist(de_parameters$reference_samples)
    comparison_samples <- unlist(de_parameters$comparison_samples)

    if ((length(reference_samples) + length(comparison_samples)) < 3 || length(reference_samples) < 1 || length(comparison_samples) < 1) {
      raise(error_bad_request("Invalid sample selections", "Must select at least 3 samples in total, with at least 1 in each group"))
    }

    if (length(intersect(reference_samples, comparison_samples)) > 0) {
      raise(error_bad_request("Invalid sample selections", "Samples cannot be in both reference and comparison groups"))
    }

    missing_reference <- setdiff(reference_samples, config$sample_columns)
    if (length(missing_reference) > 0) {
      raise(error_bad_request("Invalid reference_samples", paste("The following reference samples are not in the sample_columns:", paste(missing_reference, collapse = ", "))))
    }
    missing_comparison <- setdiff(comparison_samples, config$sample_columns)
    if (length(missing_comparison) > 0) {
      raise(error_bad_request("Invalid comparison_samples", paste("The following comparison samples are not in the sample_columns:", paste(missing_comparison, collapse = ", "))))
    }

    if (is.null(de_parameters$alpha)) {
      de_parameters$alpha <- 0.05
    } else if (!is.numeric(de_parameters$alpha) || de_parameters$alpha <= 0 || de_parameters$alpha >= 1) {
      raise(error_bad_request("Invalid alpha", "alpha must be a numeric value between 0 and 1"))
    }

    if (is.null(de_parameters$gene_filter)) {
      de_parameters$gene_filter <- list(
        type = "filter1",
        min_count_sum = 10
      )
    } else {
      valid_filter_types <- c("none", "filter1", "filter2")
      if (!(de_parameters$gene_filter$type %in% valid_filter_types)) {
        raise(error_bad_request("Invalid gene_filter type", "Must be one of: none, filter1, filter2"))
      }

      if (de_parameters$gene_filter$type == "filter1") {
        filter1_fields <- c("type", "min_count_sum")
        missing_filter1 <- setdiff(filter1_fields, names(de_parameters$gene_filter))
        if (length(missing_filter1) > 0) {
          raise(error_bad_request("Invalid gene_filter configuration", paste("Missing fields for filter1:", paste(missing_filter1, collapse = ", "))))
        }
      } else if (de_parameters$gene_filter$type == "filter2") {
        filter2_fields <- c("type", "min_count_per_sample", "min_samples")
        missing_filter2 <- setdiff(filter2_fields, names(de_parameters$gene_filter))
        if (length(missing_filter2) > 0) {
          raise(error_bad_request("Invalid gene_filter configuration", paste("Missing fields for filter2:", paste(missing_filter2, collapse = ", "))))
        }
      }
    }

    advanced_design <- de_parameters$advanced_design
    if (!is.null(advanced_design)) {
      if (!sample_sheet_exists(analysis_id)) {
        raise(error_bad_request("Advanced design requires a sample sheet", "Upload a sample sheet first via /upload-sample-sheet."))
      }

      sample_sheet <- load_sample_sheet(analysis_id)
      condition_column <- advanced_design$condition_column
      reference_level <- advanced_design$reference_level
      comparison_level <- advanced_design$comparison_level

      # Condition column is optional: sample sheets without a usable condition
      # column can still use covariates / blocking with manual group selection.
      has_condition <- !is.null(condition_column) && nzchar(condition_column)

      if (has_condition) {
        if (!condition_column %in% names(sample_sheet)) {
          raise(error_bad_request(
            "Condition column not found",
            paste0("Condition column '", condition_column, "' not found in sample sheet. Available columns: ", paste(names(sample_sheet), collapse = ", "))
          ))
        }

        condition_levels <- unique(sample_sheet[[condition_column]])
        condition_levels <- condition_levels[!is.na(condition_levels)]

        if (length(condition_levels) < 2) {
          raise(error_bad_request(
            "Condition column must have at least 2 levels",
            paste0("Condition column '", condition_column, "' has only ", length(condition_levels), " level(s).")
          ))
        }

        if (is.null(reference_level)) {
          raise(error_bad_request("Missing reference_level", "reference_level is required when using advanced design with a condition column."))
        }
        if (is.null(comparison_level)) {
          raise(error_bad_request("Missing comparison_level", "comparison_level is required when using advanced design with a condition column."))
        }

        if (!reference_level %in% condition_levels) {
          raise(error_bad_request(
            "Reference level not found",
            paste0("Reference level '", reference_level, "' not found in condition column. Available levels: ", paste(condition_levels, collapse = ", "))
          ))
        }

        if (!comparison_level %in% condition_levels) {
          raise(error_bad_request(
            "Comparison level not found",
            paste0("Comparison level '", comparison_level, "' not found in condition column. Available levels: ", paste(condition_levels, collapse = ", "))
          ))
        }

        if (reference_level == comparison_level) {
          raise(error_bad_request("Reference and comparison levels must be different"))
        }
      } else {
        condition_column <- NULL
        reference_level <- NULL
        comparison_level <- NULL
      }

      covariates <- advanced_design$covariates %||% character(0)
      blocking_column <- advanced_design$blocking_column %||% ""

      if (length(covariates) > 0) {
        missing_covs <- setdiff(covariates, names(sample_sheet))
        if (length(missing_covs) > 0) {
          raise(error_bad_request(
            "Covariate columns not found",
            paste0("Covariate columns not found in sample sheet: ", paste(missing_covs, collapse = ", "))
          ))
        }
      }

      if (blocking_column != "") {
        if (!blocking_column %in% names(sample_sheet)) {
          raise(error_bad_request(
            "Blocking column not found",
            paste0("Blocking column '", blocking_column, "' not found in sample sheet.")
          ))
        }

        blocking_values <- sample_sheet[[blocking_column]]
        if (length(unique(blocking_values)) == length(blocking_values)) {
          raise(error_bad_request(
            "Blocking column has all unique values",
            paste0("Blocking column '", blocking_column, "' has all unique values. Paired designs require repeated values (e.g., patient IDs).")
          ))
        }
      }

      if (has_condition) {
        collinear_vars <- check_collinearity(sample_sheet, condition_column, reference_level, comparison_level, covariates, blocking_column)
        if (length(collinear_vars$dropped) > 0) {
        }
        validConfig$de_parameters$advanced_design$collinear_dropped <- collinear_vars$dropped
      } else {
        # Without a condition column, check collinearity against group membership
        # of the selected reference/comparison samples.
        possible_id_cols <- c(
          "sample", "sample_id", "sampleid", "Sample", "Sample_ID", "sampleID",
          "id", "ID", "SampleID", "sample_name", "SampleName", "sample.name"
        )
        sample_id_col <- NULL
        for (col in possible_id_cols) {
          if (col %in% names(sample_sheet)) {
            sample_id_col <- col
            break
          }
        }
        if (is.null(sample_id_col)) sample_id_col <- names(sample_sheet)[1]

        ref_samples <- unlist(de_parameters$reference_samples)
        comp_samples <- unlist(de_parameters$comparison_samples)
        selected_samples <- c(ref_samples, comp_samples)
        filtered_sheet <- sample_sheet[match(selected_samples, sample_sheet[[sample_id_col]]), , drop = FALSE]
        group_condition <- factor(
          c(rep("Group1", length(ref_samples)), rep("Group2", length(comp_samples))),
          levels = c("Group1", "Group2")
        )
        collinear_vars <- check_collinearity_with_condition(
          filtered_sheet, group_condition, covariates, blocking_column
        )
        if (length(collinear_vars$dropped) > 0) {
        }
        validConfig$de_parameters$advanced_design$collinear_dropped <- collinear_vars$dropped
      }

      # Normalize optional condition fields for downstream consumers
      if (!has_condition) {
        validConfig$de_parameters$advanced_design$condition_column <- NULL
        validConfig$de_parameters$advanced_design$reference_level <- NULL
        validConfig$de_parameters$advanced_design$comparison_level <- NULL
      }
    }

    validConfig$de_parameters <- de_parameters
  }

  # Array-typed fields must survive jsonlite auto_unbox on write: a length-1
  # group or covariate list would otherwise be persisted as a bare string and
  # break strict clients. I() keeps them arrays.
  if (!is.null(validConfig$sample_columns)) {
    validConfig$sample_columns <- I(as.character(validConfig$sample_columns))
  }
  if (!is.null(validConfig$de_parameters)) {
    if (!is.null(validConfig$de_parameters$reference_samples)) {
      validConfig$de_parameters$reference_samples <-
        I(as.character(unlist(validConfig$de_parameters$reference_samples)))
    }
    if (!is.null(validConfig$de_parameters$comparison_samples)) {
      validConfig$de_parameters$comparison_samples <-
        I(as.character(unlist(validConfig$de_parameters$comparison_samples)))
    }
    ad <- validConfig$de_parameters$advanced_design
    if (!is.null(ad) && !is.null(ad$covariates)) {
      ad$covariates <- I(as.character(unlist(ad$covariates)))
      validConfig$de_parameters$advanced_design <- ad
    }
  }

  validConfig
}

# Validate and normalize the POST /v1/analyses/{analysis_id}/explore-results body.
validate_explore_parameters <- function(body) {
  if (is.null(body)) raise(error_bad_request("Missing request body"))

  organism <- body$organism %||% "Mus musculus"
  valid_organisms <- c("Mus musculus", "Homo sapiens")
  if (!(organism %in% valid_organisms)) {
    raise(error_bad_request("Invalid organism", "Must be 'Mus musculus' or 'Homo sapiens'"))
  }

  name_column <- body$name_column
  if (is.null(name_column) || !is.character(name_column) || length(name_column) == 0 || name_column == "") {
    raise(error_bad_request("Missing name_column", "name_column must name the column with gene identifiers"))
  }

  sample_columns <- as.character(unlist(body$sample_columns))
  if (length(sample_columns) == 0) {
    raise(error_bad_request("Invalid sample_columns", "At least one sample column must be provided"))
  }

  ep <- body$explore_parameters %||% list()

  transformation <- ep$transformation %||% "vst"
  if (!(transformation %in% c("none", "log2", "vst", "rlog"))) {
    raise(error_bad_request("Invalid transformation", "Must be one of: none, log2, vst, rlog"))
  }

  normalization <- ep$normalization %||% "deseq2"
  if (!(normalization %in% c("none", "deseq2", "tmm", "cpm"))) {
    raise(error_bad_request("Invalid normalization", "Must be one of: none, deseq2, tmm, cpm"))
  }

  pseudocount <- ep$pseudocount %||% 1
  if (!is.numeric(pseudocount) || length(pseudocount) != 1 || is.na(pseudocount) || pseudocount < 0) {
    raise(error_bad_request("Invalid pseudocount", "pseudocount must be a number >= 0"))
  }

  gene_filter <- ep$gene_filter %||% list(type = "filter1", min_count_sum = 10)
  filter_type <- gene_filter$type %||% "filter1"
  if (!(filter_type %in% c("none", "filter1", "filter2"))) {
    raise(error_bad_request("Invalid gene_filter type", "Must be one of: none, filter1, filter2"))
  }

  if (filter_type == "filter1") {
    if (is.null(gene_filter$min_count_sum)) {
      raise(error_bad_request("Invalid gene_filter configuration", "Missing fields for filter1: min_count_sum"))
    }
    if (!is.numeric(gene_filter$min_count_sum) || is.na(gene_filter$min_count_sum) || gene_filter$min_count_sum < 0) {
      raise(error_bad_request("Invalid gene_filter configuration", "min_count_sum must be a number >= 0"))
    }
    gene_filter <- list(
      type = "filter1",
      min_count_sum = as.numeric(gene_filter$min_count_sum)
    )
  } else if (filter_type == "filter2") {
    missing_fields <- setdiff(c("min_count_per_sample", "min_samples"), names(gene_filter))
    if (length(missing_fields) > 0) {
      raise(error_bad_request("Invalid gene_filter configuration", paste("Missing fields for filter2:", paste(missing_fields, collapse = ", "))))
    }
    if (!is.numeric(gene_filter$min_count_per_sample) || is.na(gene_filter$min_count_per_sample) || gene_filter$min_count_per_sample < 0) {
      raise(error_bad_request("Invalid gene_filter configuration", "min_count_per_sample must be a number >= 0"))
    }
    if (!is.numeric(gene_filter$min_samples) || is.na(gene_filter$min_samples) || gene_filter$min_samples < 1) {
      raise(error_bad_request("Invalid gene_filter configuration", "min_samples must be a number >= 1"))
    }
    gene_filter <- list(
      type = "filter2",
      min_count_per_sample = as.numeric(gene_filter$min_count_per_sample),
      min_samples = as.numeric(gene_filter$min_samples)
    )
  } else {
    gene_filter <- list(type = "none")
  }

  variance_filter <- ep$variance_filter %||% list()
  vf_mode <- variance_filter$mode %||% "percent"
  if (!(vf_mode %in% c("percent", "count"))) {
    raise(error_bad_request("Invalid variance_filter mode", "Must be one of: percent, count"))
  }
  vf_value <- variance_filter$value %||% 100
  if (!is.numeric(vf_value) || length(vf_value) != 1 || is.na(vf_value) || vf_value < 1) {
    raise(error_bad_request("Invalid variance_filter value", "value must be a number >= 1"))
  }

  list(
    organism = organism,
    name_column = name_column,
    sample_columns = sample_columns,
    explore_parameters = list(
      transformation = transformation,
      normalization = normalization,
      pseudocount = as.numeric(pseudocount),
      gene_filter = gene_filter,
      variance_filter = list(
        mode = vf_mode,
        value = as.numeric(vf_value)
      )
    )
  )
}

check_collinearity <- function(sample_sheet, condition_column, reference_level, comparison_level, covariates = character(0), blocking_column = "") {
  ref_mask <- sample_sheet[[condition_column]] == reference_level
  comp_mask <- sample_sheet[[condition_column]] == comparison_level
  condition <- factor(
    ifelse(ref_mask, "Group1", ifelse(comp_mask, "Group2", NA)),
    levels = c("Group1", "Group2")
  )
  check_collinearity_with_condition(sample_sheet, condition, covariates, blocking_column)
}

check_collinearity_with_condition <- function(sample_sheet, condition, covariates = character(0), blocking_column = "") {
  result <- list(dropped = character(0))

  vars_to_check <- covariates
  if (blocking_column != "") {
    vars_to_check <- unique(c(vars_to_check, blocking_column))
  }

  if (length(vars_to_check) == 0) {
    return(result)
  }

  for (var in vars_to_check) {
    if (!(var %in% names(sample_sheet))) next

    cov_values <- sample_sheet[[var]]
    if (length(cov_values) == 0 || all(is.na(cov_values))) next

    if (length(unique(cov_values)) <= 1) next

    is_perfectly_correlated <- FALSE

    if (is.numeric(cov_values)) {
      is_perfectly_correlated <- TRUE
      for (level in levels(condition)) {
        mask <- condition == level & !is.na(condition)
        cov_subset <- cov_values[mask]
        if (length(cov_subset) == 0) next
        if (length(unique(cov_subset)) > 1) {
          is_perfectly_correlated <- FALSE
          break
        }
        const_val <- cov_subset[1]
        if (length(levels(condition)) > 1) {
          for (other_level in levels(condition)) {
            if (other_level != level) {
              other_mask <- condition == other_level & !is.na(condition)
              if (any(cov_values[other_mask] == const_val, na.rm = TRUE)) {
                is_perfectly_correlated <- FALSE
                break
              }
            }
          }
        }
        if (!is_perfectly_correlated) break
      }
    } else {
      cov_factor <- factor(as.character(cov_values))
      valid_mask <- !is.na(condition) & !is.na(cov_factor)
      if (sum(valid_mask) == 0) next
      cov_condition_table <- table(cov_factor[valid_mask], condition[valid_mask])
      cov_condition_table <- cov_condition_table[rowSums(cov_condition_table) > 0, colSums(cov_condition_table) > 0, drop = FALSE]
      if (nrow(cov_condition_table) > 0 && ncol(cov_condition_table) > 0) {
        row_sums_binary <- rowSums(cov_condition_table > 0)
        col_sums_binary <- colSums(cov_condition_table > 0)
        if (all(row_sums_binary == 1) && all(col_sums_binary == 1) && nrow(cov_condition_table) == ncol(cov_condition_table)) {
          is_perfectly_correlated <- TRUE
        }
      }
    }

    if (is_perfectly_correlated) {
      result$dropped <- c(result$dropped, var)
    }
  }

  result
}

parse_collection_value <- function(collection_value) {
  if (is.null(collection_value) || length(collection_value) == 0 || collection_value == "") {
    stop("No gene set category selected. Please choose a category.")
  }

  if (grepl("^MGI_GO_", collection_value)) {
    collection <- "MGI_GO"
    value <- sub("MGI_GO_", "", collection_value)
    value <- switch(value,
      "BP+MF+CC" = "BP+MF+CC",
      "BP" = "BP",
      "CC" = "CC",
      "MF" = "MF",
      stop("Invalid MGI GO value")
    )
  } else if (grepl("^GOA_GO_", collection_value)) {
    collection <- "GOA_GO"
    value <- sub("GOA_GO_", "", collection_value)
    value <- switch(value,
      "BP+MF+CC" = "BP+MF+CC",
      "BP" = "BP",
      "CC" = "CC",
      "MF" = "MF",
      stop("Invalid GOA GO value")
    )
  } else if (grepl("^MSigDB_GO_", collection_value)) {
    collection <- "MSigDB_GO"
    value <- sub("MSigDB_GO_", "", collection_value)
    value <- switch(value,
      "BP+MF+CC" = "BP+MF+CC",
      "BP+MF+CC+HP" = "BP+MF+CC+HP",
      "BP" = "BP",
      "CC" = "CC",
      "MF" = "MF",
      stop("Invalid MSigDB GO value")
    )
  } else if (collection_value == "MSigDB_MP") {
    collection <- "MSigDB_MP"
    value <- NULL
  } else if (collection_value == "MSigDB_HP") {
    collection <- "MSigDB_HP"
    value <- NULL
  } else if (collection_value == "MSigDB_GTRD") {
    collection <- "MSigDB_GTRD"
    value <- NULL
  } else if (collection_value == "MSigDB_MIRDB") {
    collection <- "MSigDB_MIRDB"
    value <- NULL
  } else if (collection_value == "MSigDB_MIR_MIRDB") {
    collection <- "MSigDB_MIR_MIRDB"
    value <- NULL
  } else if (collection_value == "MSigDB_MIR_MIR_LEGACY") {
    collection <- "MSigDB_MIR_MIR_LEGACY"
    value <- NULL
  } else if (collection_value == "MSigDB_TFT_GTRD") {
    collection <- "MSigDB_TFT_GTRD"
    value <- NULL
  } else if (collection_value == "MSigDB_TFT_TFT_LEGACY") {
    collection <- "MSigDB_TFT_TFT_LEGACY"
    value <- NULL
  } else if (collection_value == "MSigDB_BIOCARTA") {
    collection <- "MSigDB_BIOCARTA"
    value <- NULL
  } else if (collection_value == "MSigDB_KEGG_MEDICUS") {
    collection <- "MSigDB_KEGG_MEDICUS"
    value <- NULL
  } else if (collection_value == "MSigDB_PID") {
    collection <- "MSigDB_PID"
    value <- NULL
  } else if (collection_value == "MSigDB_KEGG_LEGACY") {
    collection <- "MSigDB_KEGG_LEGACY"
    value <- NULL
  } else if (collection_value == "MSigDB_CGP") {
    collection <- "MSigDB_CGP"
    value <- NULL
  } else if (collection_value == "MSigDB_Reactome") {
    collection <- "MSigDB_Reactome"
    value <- NULL
  } else if (collection_value == "MSigDB_WikiPathways") {
    collection <- "MSigDB_WikiPathways"
    value <- NULL
  } else if (collection_value == "MSigDB_CELL_TYPE") {
    collection <- "MSigDB_CELL_TYPE"
    value <- NULL
  } else if (collection_value == "MSigDB_Hallmark") {
    collection <- "MSigDB_Hallmark"
    value <- NULL
  } else {
    stop("Invalid collection selection")
  }
  list(collection = collection, value = value)
}
