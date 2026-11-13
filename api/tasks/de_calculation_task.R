source("api/helpers/sample_sheet_helpers.R")

perform_de_calculation <- \(organism, model, sig_params, count_matrix, coldata, de_summary = NULL, advanced_de_params = NULL) {
  if (model == "DESeq2") {
    result <- calc_with_deseq2(
      organism = organism,
      sig_params = sig_params,
      count_matrix = count_matrix,
      coldata = coldata,
      advanced_de_params = advanced_de_params,
      de_summary = de_summary
    )
  } else if (model == "DESeq2 (Rust)") {
    result <- calc_with_deseq2_rust(
      organism = organism,
      sig_params = sig_params,
      count_matrix = count_matrix,
      coldata = coldata,
      advanced_de_params = advanced_de_params,
      de_summary = de_summary
    )
  } else if (model == "Limma-Voom") {
    result <- calc_with_limmavoom(
      organism = organism,
      sig_params = sig_params,
      count_matrix = count_matrix,
      coldata = coldata,
      advanced_de_params = advanced_de_params,
      de_summary = de_summary
    )
  } else {
    return(list(
      status = "error",
      error = "Error: Invalid model selection"
    ))
  }

  result
}

calc_with_deseq2 <- \(organism, sig_params, count_matrix, coldata, advanced_de_params = NULL, de_summary = NULL) {
  useAdvancedDesign <- !is.null(advanced_de_params)

  # R DESeq2 can not process fractions, so we round
  # Note that counts are already validated to be NOT normalized upon upload.
  # Hence rounding only happens in the case when user provides specific unnormalized
  # data with fractions
  count_matrix <- round(count_matrix)
  storage.mode(count_matrix) <- "integer"

  if (useAdvancedDesign) {
    condition_column <- advanced_de_params$condition_column
    reference_level <- advanced_de_params$reference_level
    comparison_level <- advanced_de_params$comparison_level
    covariates <- advanced_de_params$covariates
    blocking_column <- advanced_de_params$blocking_column

    design_formula <- buildDESeq2Design(covariates, blocking_column)
    contrast <- c(condition_column, comparison_level, reference_level)
  } else {
    design_formula <- ~condition
    contrast <- c("condition", "Group2", "Group1")
  }

  dds <- tryCatch(
    {
      DESeq2::DESeqDataSetFromMatrix(
        countData = count_matrix,
        colData = coldata,
        design = design_formula
      )
    },
    error = \(e) list(status = "error", error = conditionMessage(e))
  )
  if (is.list(dds) && !is.null(dds$status) && dds$status == "error") return(dds)

  dds <- tryCatch(
    DESeq2::DESeq(dds, quiet = TRUE),
    error = \(e) list(status = "error", error = conditionMessage(e))
  )

  if (is.list(dds) && !is.null(dds$status) && dds$status == "error") return(dds)

  norm_counts <- DESeq2::counts(dds, normalized = TRUE)
  norm_counts_df <- as.data.frame(norm_counts)
  norm_counts_df <- cbind(gene_id = rownames(norm_counts_df), norm_counts_df)

  dispersion_data_df <- tryCatch({
    base_means <- S4Vectors::mcols(dds)$baseMean
    dispersion_values <- S4Vectors::mcols(dds)$dispGeneEst
    fitted_dispersion_fn <- DESeq2::dispersionFunction(dds)

    min_length <- min(length(base_means), length(dispersion_values))
    base_means <- base_means[1:min_length]
    dispersion_values <- dispersion_values[1:min_length]

    # Calculate fitted values at the specific baseMeans points
    log_base_means <- log10(base_means)
    fitted_values <- fitted_dispersion_fn(log_base_means)

    data.frame(
      baseMean = base_means,
      dispersion = dispersion_values,
      fitted_dispersion = fitted_values,
      model = "DESeq2",
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    NULL
  })


  vsdata_temp <- DESeq2::varianceStabilizingTransformation(dds, blind = FALSE)
  vst_mat <- SummarizedExperiment::assay(vsdata_temp)
  colnames(vst_mat) <- colnames(norm_counts)
  vst_mat_df <- as.data.frame(vst_mat)
  vst_mat_df <- cbind(gene_id = rownames(vst_mat_df), vst_mat_df)
  rm(vsdata_temp)

  res <- DESeq2::results(
    dds,
    contrast = contrast,
    alpha = sig_params$alpha,
    lfcThreshold = sig_params$lfcThreshold,
    altHypothesis = sig_params$altHypothesis
  )

  # Track DESeq2 independent filtering info
  independent_filter_removed <- 0
  independent_filter_threshold <- NULL
  res_metadata <- S4Vectors::metadata(res)
  if (!is.null(res_metadata$filterNumRej)) independent_filter_removed <- res_metadata$filterNumRej
  if (!is.null(res_metadata$filterThreshold)) independent_filter_threshold <- res_metadata$filterThreshold

  # Count outliers and independent filtered genes from results
  # Independent filtered: NA padj but existent pvalue
  # Outliers: NA pvalue (and thus NA padj)
  na_pval <- is.na(res$pvalue)
  na_padj <- is.na(res$padj)
  outlier_count <- sum(na_pval)
  indep_filtered_count <- sum(!na_pval & na_padj)

  # Apply LFC shrinkage
  # Determine coef name for apeglm based on design
  if (useAdvancedDesign) {
    coef_name <- paste0(condition_column, "_", comparison_level, "_vs_", reference_level)
  } else {
    coef_name <- "condition_Group2_vs_Group1"
  }

  resLFC <- DESeq2::lfcShrink(
    dds,
    res = res,
    type = "apeglm",
    coef = coef_name,
    quiet = TRUE
  )

  res$log2FoldChange <- resLFC$log2FoldChange
  res$stat <- res$stat
  rm(resLFC)

  sigs_df <- as.data.frame(res)
  sigs_df$gene_id <- rownames(sigs_df)
  names(sigs_df)[names(sigs_df) == "log2FoldChange"] <- "log2FC"

  sigs_df <- sigs_df |>
    dplyr::select(gene_id, pvalue, padj, log2FC, lfcSE, baseMean, stat)

  de_summary <- de_summary %||% list()
  de_summary$genes_in_results <- nrow(sigs_df)
  de_summary$removed_by_independent_filtering <- indep_filtered_count
  de_summary$independent_filter_threshold <- independent_filter_threshold
  de_summary$outliers <- outlier_count
  de_summary$model_used <- "DESeq2"

  list(
    sigs = sigs_df,
    norm_counts = norm_counts_df,
    dispersion_data = dispersion_data_df,
    vst_mat = vst_mat_df,
    status = "success",
    de_summary = de_summary
  )
}

# =========================================================
# ==================== Limma-Voom =========================
# =========================================================
calc_with_limmavoom <- \(organism, sig_params, count_matrix, coldata, advanced_de_params = NULL, de_summary = NULL) {
  useAdvancedDesign <- !is.null(advanced_de_params)

  if (useAdvancedDesign) {
    conditionColumn <- advanced_de_params$condition_column
    referenceLevel <- advanced_de_params$reference_level
    comparisonLevel <- advanced_de_params$comparison_level
    covariates <- advanced_de_params$covariates
    blockingColumn <- advanced_de_params$blocking_column
  }

  # Limma-Voom analysis - wrap in tryCatch
  limma_result <- tryCatch(
    {
      dge <- DGEList(counts = count_matrix, group = coldata$condition)
      dge <- calcNormFactors(dge)

      if (useAdvancedDesign) {
        terms <- paste0("0 + ", conditionColumn)
        if (!is.null(covariates) && length(covariates) > 0) {
          terms <- paste(terms, paste(covariates, collapse = " + "), sep = " + ")
        }
        if (!is.null(blockingColumn) && blockingColumn != "") {
          terms <- paste(terms, blockingColumn, sep = " + ")
        }

        design_formula <- as.formula(paste("~", terms))
        design <- model.matrix(design_formula, data = coldata)

        # Find the design matrix column names that correspond to our condition levels
        # R encodes factor levels as: columnNamelevel (e.g., "conditioncontrol")
        col_names <- colnames(design)
        ref_col <- paste0(conditionColumn, referenceLevel)
        comp_col <- paste0(conditionColumn, comparisonLevel)

        # Verify the columns exist
        if (!(ref_col %in% col_names) || !(comp_col %in% col_names)) {
          stop(paste(
            "Cannot find condition columns in design matrix. Looking for:", ref_col, "and", comp_col,
            "Available columns:", paste(col_names, collapse = ", ")
          ))
        }

        # Build contrast: comparison - reference
        contrast_name <- paste0(comp_col, "-", ref_col)
        contrast <- makeContrasts(contrasts = contrast_name, levels = design)

        # Limma uses the condition column for group specification
        dge$samples$group <- coldata[[conditionColumn]]
      } else {
        # Simple design matrix
        design <- model.matrix(~ 0 + condition, data = coldata)
        colnames(design) <- levels(coldata$condition)
        contrast <- makeContrasts(contrasts = "Group2-Group1", levels = design)
      }

      # voom transformation
      v <- voom(dge, design, plot = FALSE)

      # Fit linear model
      fit <- lmFit(v, design)
      fit2 <- contrasts.fit(fit, contrast)
      fit2 <- eBayes(fit2)

      # Get results
      res <- topTable(fit2, coef = 1, number = Inf, adjust.method = "BH", sort.by = "P")

      # Rename columns to match DESeq2 naming (t -> stat so GSEA ranking,
      # the stat filter, and the results table all work for Limma too)
      colnames(res) <- c("log2FC", "AveExpr", "stat", "pvalue", "padj", "B")

      # fst drops rownames, so carry gene_id explicitly (rownames kept for enrichment)
      sigs <- as.data.frame(res)
      sigs$gene_id <- rownames(sigs)
      sigs <- sigs |> dplyr::select(gene_id, pvalue, padj, log2FC, AveExpr, stat, B)

      list(status = "success", sigs = sigs, v = v)
    },
    error = function(e) {
      list(status = "error", error = conditionMessage(e))
    }
  )

  if (limma_result$status == "error") {
    return(limma_result)
  }

  # Extract results from successful run
  sigs <- limma_result$sigs
  v <- limma_result$v

  # fst requires data frames; voom's E matrix carries gene IDs as rownames only,
  # so carry them as an explicit gene_id column (consumers read gene_id, not rownames)
  norm_counts_df <- as.data.frame(v$E)
  norm_counts_df <- cbind(gene_id = rownames(norm_counts_df), norm_counts_df)
  vst_mat_df <- as.data.frame(v$E)
  vst_mat_df <- cbind(gene_id = rownames(vst_mat_df), vst_mat_df)

  de_summary <- de_summary %||% list()
  de_summary$genes_in_results <- nrow(sigs)
  de_summary$removed_by_independent_filtering <- 0
  de_summary$independent_filter_threshold <- NULL
  de_summary$outliers <- 0
  de_summary$model_used <- "Limma-Voom"

  result <- list(
    sigs = sigs,
    norm_counts = norm_counts_df,
    dispersion_data = NULL, # Limma-Voom doesn't have dispersion data like DESeq2
    vst_mat = vst_mat_df, # voom transformed values
    status = "success",
    de_summary = de_summary
  )
}

# =========================================================
# ==================== Rust-backed DESeq2 =================
# =========================================================
# Many times faster, but not as cited and popular as R DESeq2.
# Although Rust version of DESeq2 (rust_deseq2 crate by @necoli1822)
# implements exatly the same paper as R DESeq2, Rust language itself
# has minor differences in the way it computes numbers.
# This leads to slight differences in resulting values.
# This backend uses an R wrapper around rust_deseq2 crate.
# More info here:
# Underlying crate - https://github.com/necoli1822/rust_deseq2
# R wrapper - https://github.com/l-ko/RustDESeq2
calc_with_deseq2_rust <- \(organism, sig_params, count_matrix, coldata, advanced_de_params = NULL, de_summary = NULL) {
  if (!requireNamespace("RustDESeq2", quietly = TRUE)) {
    return(list(
      status = "error",
      error = 'R package "RustDESeq2" is not installed. Please install it first.'
    ))
  }

  # Rust version can process fractions, but we round here for consistecy
  # Note that counts are already validated to be NOT normalized upon upload
  # Hence rounding only happens in the case when user provides specific unnormalized
  # data with fractions
  deseq_count_matrix <- round(count_matrix)
  storage.mode(deseq_count_matrix) <- "integer"
  dds <- RustDESeq2::deseq_dataset_from_matrix(
    count_data = deseq_count_matrix,
    gene_ids = rownames(count_matrix),
    sample_ids = colnames(count_matrix),
    condition = as.character(coldata$condition),
    design_variable = "condition"
  )

  dds <- RustDESeq2::deseq(dds)

  norm_counts <- as.matrix(RustDESeq2::counts(dds, normalized = TRUE))
  if (is.null(rownames(norm_counts))) rownames(norm_counts) <- rownames(count_matrix)

  # Extract dispersion data
  dispersion_data <- NULL
  tryCatch(
    {
      final_dispersions <- RustDESeq2::dispersions(dds)
      dispersion_fn <- RustDESeq2::dispersion_function(dds)
      base_means <- RustDESeq2::base_mean(dds)

      if (!is.null(final_dispersions) && !is.null(base_means)) {
        dispersion_values <- as.numeric(final_dispersions)
        base_mean_values <- as.numeric(base_means)

        min_length <- min(length(dispersion_values), length(base_mean_values))
        if (length(dispersion_values) != length(base_mean_values)) {
          dispersion_values <- dispersion_values[1:min_length]
          base_mean_values <- base_mean_values[1:min_length]
        }

        fitted_dispersion <- tryCatch(
          {
            if (is.function(dispersion_fn)) {
              as.numeric(dispersion_fn(log10(base_mean_values)))
            } else if (is.list(dispersion_fn) &&
                       is.numeric(dispersion_fn$asymptotic_dispersion) &&
                       is.numeric(dispersion_fn$extra_poisson)) {
              dispersion_fn$asymptotic_dispersion + dispersion_fn$extra_poisson / base_mean_values
            } else {
              rep(NA_real_, min_length)
            }
          },
          error = function(e) rep(NA_real_, min_length)
        )
        if (!(length(fitted_dispersion) %in% c(1L, min_length))) {
          fitted_dispersion <- rep(NA_real_, min_length)
        }

        dispersion_data <- data.frame(
          baseMean = base_mean_values,
          dispersion = dispersion_values,
          fitted_dispersion = fitted_dispersion,
          model = "DESeq2_Rust",
          stringsAsFactors = FALSE
        )
      }
    },
    error = function(e) {
      dispersion_data <<- NULL
    }
  )

  gene_ids <- rownames(norm_counts)

  # Compute VST data
  vsdata_temp <- RustDESeq2::vst_transform(dds, blind = FALSE)
  vst_mat <- RustDESeq2::assay(vsdata_temp, assay_name = NULL)

  if (is.array(vst_mat)) {
    vst_mat <- as.matrix(vst_mat)
  }
  if (is.null(rownames(vst_mat)) && length(gene_ids) == nrow(vst_mat)) {
    rownames(vst_mat) <- gene_ids
  }
  if (is.null(colnames(vst_mat)) && length(rownames(coldata)) == ncol(vst_mat)) {
    colnames(vst_mat) <- rownames(coldata)
  }
  rm(vsdata_temp)

  useAdvancedDesign <- !is.null(advanced_de_params)
  if (useAdvancedDesign) {
    numerator <- advanced_de_params$comparison_level
    denominator <- advanced_de_params$reference_level
  } else {
    numerator <- "Group2"
    denominator <- "Group1"
  }

  res <- RustDESeq2::results(
    dds,
    numerator = numerator,
    denominator = denominator,
    alpha = sig_params$alpha
  )

  sigs <- as.data.frame(res)
  rownames(sigs) <- rownames(count_matrix)
  if ("log2FoldChange" %in% names(sigs)) names(sigs)[names(sigs) == "log2FoldChange"] <- "log2FC"

  sigs$gene_id <- rownames(sigs)
  sigs <- sigs |> dplyr::select(gene_id, pvalue, padj, log2FC, lfcSE, baseMean, stat)

  de_summary <- de_summary %||% list()
  de_summary$genes_in_results <- nrow(sigs)
  # RustDESeq2 doesn't provide the same independent filtering metadata
  de_summary$removed_by_independent_filtering <- 0
  if (useAdvancedDesign) {
    de_summary$model_used <- "DESeq2 (Rust) - Note: Advanced covariates not used with RustDESeq2"
  } else {
    de_summary$model_used <- "DESeq2 (Rust)"
  }

  # fst requires data frames with an explicit gene_id column (fst drops rownames)
  norm_counts_df <- as.data.frame(norm_counts)
  norm_counts_df <- cbind(gene_id = rownames(norm_counts_df), norm_counts_df)
  vst_mat_df <- as.data.frame(vst_mat)
  vst_mat_df <- cbind(gene_id = rownames(vst_mat_df), vst_mat_df)

  list(
    sigs = sigs,
    norm_counts = norm_counts_df,
    dispersion_data = dispersion_data,
    vst_mat = vst_mat_df,
    status = "success",
    de_summary = de_summary
  )
}
