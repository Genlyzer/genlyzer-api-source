# Monitor-side DE task -------------------------------------------------------
# Reads counts and design from the analysis, submits the DESeq2/limma
# calculation to a mirai worker and persists the result. The math itself
# lives in api/tasks/de_calculation_task.R and runs inside the worker.

prepare_coldata <- function(reference_samples, comparison_samples) {
  condition <- c(
    rep("Group1", length(reference_samples)),
    rep("Group2", length(comparison_samples))
  )
  sample_names <- c(reference_samples, comparison_samples)
  data.frame(
    condition = factor(condition),
    row.names = sample_names,
    stringsAsFactors = FALSE
  )
}

prepare_sig_params <- function(config) {
  sigParams <- list()
  sigParams$alpha <- config$de_parameters$alpha %||% 0.05
  sigParams$lfcThreshold <- 0
  sigParams$altHypothesis <- "greaterAbs"
  sigParams
}

start_de_task <- function(ctx) {
  task_data <- ctx$task_data
  task_item <- ctx$task_item
  analysis_id <- ctx$analysis_id
  metadata <- ctx$metadata
  config <- ctx$config
  analysis_key <- ctx$analysis_key
  current_analysis_dir <- ctx$analysis_dir

  fail_de <- function(e) {
    metadata <- get_analysis(analysis_id)
    message(sprintf("[%s] DE task failed", Sys.time()))
    update_analysis(metadata, list(
      status = "de_failed",
      running = FALSE,
      de_complete = FALSE,
      message = paste("DE analysis failed:", describe_task_error(e))
    ))
  }

  setup <- tryCatch({
    current_counts_file_path <- file.path(current_analysis_dir, COUNTS_FILE_NAME)

    de_parameters <- config$de_parameters
    reference_samples <- de_parameters$reference_samples
    comparison_samples <- de_parameters$comparison_samples
    advanced_design <- de_parameters$advanced_design

    organism <- config$organism
    model <- de_parameters$model
    sig_params <- prepare_sig_params(config)
    count_matrix <- load_count_matrix(current_counts_file_path, config, analysis_key)

    # Apply low-count gene filtering
    gene_filter <- de_parameters$gene_filter %||% list(type = "filter1", min_count_sum = 10)
    total_before <- nrow(count_matrix)
    if (gene_filter$type == "filter1") {
      keep <- rowSums(count_matrix) >= gene_filter$min_count_sum
      filter_condition <- paste0("Sum of counts >= ", gene_filter$min_count_sum)
    } else if (gene_filter$type == "filter2") {
      keep <- rowSums(count_matrix >= gene_filter$min_count_per_sample) >= gene_filter$min_samples
      filter_condition <- paste0(">= ", gene_filter$min_count_per_sample, " counts in >= ", gene_filter$min_samples, " samples")
    } else {
      keep <- rep(TRUE, nrow(count_matrix))
      filter_condition <- "No gene filtering applied"
    }
    genes_removed <- total_before - sum(keep)
    count_matrix <- count_matrix[keep, , drop = FALSE]

    # Build de_summary with count filter info
    de_summary <- list(
      total_genes_start = total_before,
      genes_after_count_filter = nrow(count_matrix),
      count_filter_applied = gene_filter$type != "none",
      count_filter_condition = filter_condition,
      removed_by_count_filter = genes_removed,
      use_advanced_design = !is.null(advanced_design)
    )

    advanced_de_params <- NULL
    if (!is.null(advanced_design)) {
      condition_column <- advanced_design$condition_column
      reference_level <- advanced_design$reference_level
      comparison_level <- advanced_design$comparison_level
      covariates <- advanced_design$covariates %||% character(0)
      blocking_column <- advanced_design$blocking_column %||% ""
      has_condition <- !is.null(condition_column) && nzchar(condition_column)

      # Load sample sheet and build coldata
      sample_sheet_path <- file.path(current_analysis_dir, "sample_sheet.fst")
      sample_sheet <- read_session_fst(sample_sheet_path, analysis_key)

      possible_id_cols <- c("sample", "sample_id", "sampleid", "Sample", "Sample_ID", "sampleID", "id", "ID", "SampleID", "sample_name", "SampleName", "sample.name")
      sample_id_col <- NULL
      for (col in possible_id_cols) {
        if (col %in% names(sample_sheet)) { sample_id_col <- col; break }
      }
      if (is.null(sample_id_col)) sample_id_col <- names(sample_sheet)[1]

      selected_samples <- c(reference_samples, comparison_samples)
      if (has_condition) {
        condition <- c(
          rep(reference_level, length(reference_samples)),
          rep(comparison_level, length(comparison_samples))
        )
        condition <- factor(condition, levels = c(reference_level, comparison_level))
      } else {
        # Manual groups without a sample-sheet condition column
        reference_level <- "Group1"
        comparison_level <- "Group2"
        condition <- c(
          rep(reference_level, length(reference_samples)),
          rep(comparison_level, length(comparison_samples))
        )
        condition <- factor(condition, levels = c(reference_level, comparison_level))
      }

      # Check for collinearity, and drop the selected covariates and blocking column if necessary
      dropped_covariates <- character(0)
      dropped_blocking <- NULL
      if (length(covariates) > 0 || (blocking_column != "" && !is.null(blocking_column))) {
        filtered_sheet <- sample_sheet[match(selected_samples, sample_sheet[[sample_id_col]]), , drop = FALSE]

        if (has_condition) {
          corr_result <- check_collinearity(
            filtered_sheet, condition_column, reference_level, comparison_level,
            covariates, blocking_column
          )
        } else {
          group_condition <- factor(
            c(rep("Group1", length(reference_samples)), rep("Group2", length(comparison_samples))),
            levels = c("Group1", "Group2")
          )
          corr_result <- check_collinearity_with_condition(
            filtered_sheet, group_condition, covariates, blocking_column
          )
        }

        dropped_all <- corr_result$dropped
        # Separate blocking column drops from covariate drops
        if (blocking_column != "" && blocking_column %in% dropped_all) {
          dropped_blocking <- blocking_column
          blocking_column <- NULL
        }
        # Remove ALL dropped variables from covariates (including ones that were also the blocking column)
        covariates <- setdiff(covariates, dropped_all)
        dropped_covariates <- setdiff(dropped_all, dropped_blocking)
      }

      coldata <- buildColdataFromSampleSheet(
        sample_sheet, sample_id_col, selected_samples,
        condition, covariates, blocking_column
      )

      # coldata always has a column named "condition" (from buildColdataFromSampleSheet)
      advanced_de_params <- list(
        condition_column = "condition",
        reference_level = reference_level,
        comparison_level = comparison_level,
        covariates = covariates,
        blocking_column = blocking_column
      )

      de_summary$covariates <- covariates
      de_summary$blocking_column <- blocking_column
      de_summary$condition_column <- if (has_condition) condition_column else "condition"
      de_summary$reference_level <- reference_level
      de_summary$comparison_level <- comparison_level
      if (length(dropped_covariates) > 0) {
        de_summary$dropped_covariates <- dropped_covariates
        de_summary$original_covariates <- advanced_design$covariates %||% character(0)
        de_summary$collinear_dropped <- dropped_covariates
      }
      if (!is.null(dropped_blocking)) {
        de_summary$dropped_blocking_column <- dropped_blocking
      }
    } else {
      coldata <- prepare_coldata(reference_samples, comparison_samples)
    }

    message("Starting DE analysis task in Mirai worker...")

    task <- mirai::mirai({
      source(DE_WORKER_SCRIPT_PATH)
      result <- perform_de_calculation(organism, model, sig_params, count_matrix, coldata, de_summary, advanced_de_params)
      list(success = TRUE, result = result, completed_at = Sys.time())
    },
    DE_WORKER_SCRIPT_PATH = DE_WORKER_SCRIPT_PATH,
    organism = organism,
    model = model,
    sig_params = sig_params,
    count_matrix = count_matrix,
    coldata = coldata,
    de_summary = de_summary,
    advanced_de_params = advanced_de_params,
    .timeout = 3600000  # 60 min timeout for DE task
    )

    list(
      task = task,
      complete = function() {
        # Merge with the latest on-disk metadata
        metadata <- get_analysis(analysis_id)
        task_result <- mirai::call_mirai(task)$data
        if (is_error_value(task_result)) {
          fail_de(task_result)
          return()
        }
        resultValue <- task_result$result

        if (is_error_value(resultValue)) {
          fail_de(resultValue)
          return()
        }
        if (!is.null(resultValue$status) && resultValue$status == "error") {
          update_analysis(metadata, list(
            status = "de_failed",
            running = FALSE,
            de_complete = FALSE,
            message = resultValue$error %||% "DE analysis failed"
          ))
          message(sprintf("[%s] DE calculation failed", Sys.time()))
        } else {
          # Enrich with gene annotations
          con <- DBI::dbConnect(RSQLite::SQLite(), dbname = getDatabasePath(organism))
          on.exit(DBI::dbDisconnect(con), add = TRUE)
          # results looks like this: list(sigs = sigs_df, norm_counts = norm_counts_df, vst_mat = vst_mat_df, dispersion_data = dispersion_data_df, de_summary = de_summary_list)
          resultValue$sigs <- enrichGeneAnnotations(resultValue$sigs, con)

          # Save first: if saving fails, the handler marks the analysis
          # de_failed instead of leaving de_complete with missing result files
          save_de_results(analysis_id, resultValue, analysis_key)

          # Mirror DEMO results into the shared content-hash cache. Private analyses run
          # with use_cache = FALSE and never enter this branch.
          de_meta_updates <- list(
            status = "de_complete",
            running = FALSE,
            de_complete = TRUE,
            message = "DE analysis completed successfully"
          )
          de_cache_key <- task_data$cache_key %||% NULL
          if (isTRUE(task_data$use_cache) &&
                !is.null(de_cache_key) && length(de_cache_key) == 1L &&
                nzchar(as.character(de_cache_key))) {
            de_cache_key <- as.character(de_cache_key)
            cache_store_files(de_cache_key, current_analysis_dir, DE_RESULT_FILES)
            de_meta_updates$de_cache_key <- de_cache_key
          }
          update_analysis(metadata, de_meta_updates)
          message(sprintf("[%s] Task completed successfully", Sys.time()))
        }
      }
    )
  }, error = function(e) {
    fail_de(e)
    NULL
  })

  if (is.null(setup)) {
    liteq::ack(task_item)
    return(FALSE)
  }
  list(task = setup$task, task_item = task_item, analysis_id = analysis_id,
       complete = setup$complete, fail = fail_de, analysis_key = analysis_key)
}
