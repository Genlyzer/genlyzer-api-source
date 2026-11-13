app$add_post(
  path = "/v1/analyses",
  FUN = function(request, response) {
    check_disk_space(1)

    demo <- parse_demo_flag(request$parameters_query$demo)

    tryCatch(
      {
        analysis <- if (demo) create_demo_analysis() else create_analysis()
      },
      error = function(e) {
        raise(error_internal_error("Failed to create analysis"))
      }
    )

    success_response(response, list(
      analysis_id = analysis$analysis_id,
      created_at = analysis$created_at,
      demo = isTRUE(analysis$demo)
    ), status_code = 201L)
  }
)

app$add_route("/v1/analyses", method = "OPTIONS", FUN = function(req, res) {
  res$set_header("Access-Control-Allow-Origin", "*")
  res$set_header("Access-Control-Allow-Methods", "POST, OPTIONS")
  res$set_header("Access-Control-Allow-Headers", CORS_ALLOWED_HEADERS)
  res$set_header("Access-Control-Max-Age", "86400")
})

# get metadata
app$add_get(
  path = "/v1/analyses/{analysis_id}",
  match = "regex",
  add_head = FALSE,
  FUN = function(request, response) {
    analysis_id <- request$parameters_path$analysis_id

    validate_analysis_id(analysis_id)
    session_key <- analysis_key_from_request(request, analysis_id)

    analysis <- get_analysis(analysis_id)
    if (is.null(analysis)) {
      return(error_not_found("Analysis not found"))
    }

    # Current analysis queue status
    is_pending <- isTRUE(analysis$running) ||
      grepl("_queued$", as.character(analysis$status %||% ""))
    if (is_pending) {
      queue_overall_counts <- tryCatch(queue_status_counts(QUEUE_DB_PATH), error = function(e) NULL)
      if (!is.null(queue_overall_counts)) {
        analysis$queue_pending <- queue_overall_counts$ready
        analysis$queue_running <- queue_overall_counts$working
      }
      current_analysis_state <- tryCatch(
        queue_task_state(analysis_id, QUEUE_DB_PATH),
        error = function(e) NA_character_
      )
      if (length(current_analysis_state) == 1L && !is.na(current_analysis_state)) {
        analysis$queue_state <- unname(current_analysis_state)
      }
    }

    success_response(response, analysis)
  }
)

app$add_route("/v1/analyses/{analysis_id}", match = "regex", method = "OPTIONS", FUN = function(req, res) {
  res$set_header("Access-Control-Allow-Origin", "*")
  res$set_header("Access-Control-Allow-Methods", "GET, OPTIONS")
  res$set_header("Access-Control-Allow-Headers", CORS_ALLOWED_HEADERS)
  res$set_header("Access-Control-Max-Age", "86400")
})

# get config (eg. for DE or Explore runs)
app$add_get(
  path = "/v1/analyses/{analysis_id}/config",
  match = "regex",
  FUN = function(request, response) {
    analysis_id <- request$parameters_path$analysis_id

    validate_analysis_id(analysis_id)
    analysis_key_from_request(request, analysis_id)

    config_file_path <- get_config_file_path(analysis_id)
    if (!file.exists(config_file_path)) {
      raise(error_not_found("Configuration not found", "Configuration has not been saved for this analysis yet."))
    }

    tryCatch(
      {
        config <- jsonlite::read_json(config_file_path, simplifyVector = FALSE)
      },
      error = function(e) {
        raise(error_internal_error("Failed to read configuration"))
      }
    )

    success_response(response, config)
  }
)

# Upload optional sample sheet for advanced DE design
app$add_post(
  path = "/v1/analyses/{analysis_id}/upload-sample-sheet",
  match = "regex",
  FUN = function(request, response) {
    analysis_id <- request$parameters_path$analysis_id
    validate_analysis_id(analysis_id)
    session_key <- analysis_key_from_request(request, analysis_id)
    analysis <- get_analysis(analysis_id)

    if (analysis$configured) {
      raise(error_unprocessable("Cannot upload sample sheet", "Analysis is already configured. If you want to upload a new sample sheet, please create a new analysis."))
    }

    content_type <- tolower(request$headers[["content-type"]] %||% "")
    if (content_type != "text/csv") {
      raise(error_bad_request("Invalid Content-Type", "Sample sheet must be uploaded as text/csv."))
    }

    result <- process_sample_sheet(request$body, analysis_id, session_key)

    success_response(response, list(
      status = "complete",
      rows = result$rows,
      columns = result$columns,
      column_names = result$column_names,
      column_levels = result$column_levels,
      sample_id_column = result$sample_id_column,
      sample_values = result$sample_values
    ), status_code = 200L)
  }
)

app$add_route("/v1/analyses/{analysis_id}/upload-sample-sheet", match = "regex", method = "OPTIONS", FUN = function(req, res) {
  res$set_header("Access-Control-Allow-Origin", "*")
  res$set_header("Access-Control-Allow-Methods", "POST, OPTIONS")
  res$set_header("Access-Control-Allow-Headers", CORS_ALLOWED_HEADERS)
  res$set_header("Access-Control-Max-Age", "86400")
})

app$add_get(
  path = "/v1/analyses/{analysis_id}/sample-sheet",
  match = "regex",
  FUN = function(request, response) {
    analysis_id <- request$parameters_path$analysis_id
    validate_analysis_id(analysis_id)
    session_key <- analysis_key_from_request(request, analysis_id)

    sheet_path <- file.path(SESSION_DATA_DIR, analysis_id, "sample_sheet.fst")
    if (!file.exists(sheet_path)) {
      raise(error_not_found(
        "Sample sheet not found",
        "Upload a sample sheet first."
      ))
    }
    sheet <- tryCatch(read_session_fst(sheet_path, session_key), error = function(e) NULL)
    if (is.null(sheet)) {
      raise(error_internal_error("Failed to read sample sheet"))
    }
    success_response(response, sample_sheet_metadata(
      sheet,
      detect_sample_id_column(sheet),
      hash_file(sheet_path)
    ))
  }
)

app$add_route("/v1/analyses/{analysis_id}/sample-sheet", match = "regex", method = "OPTIONS", FUN = function(req, res) {
  res$set_header("Access-Control-Allow-Origin", "*")
  res$set_header("Access-Control-Allow-Methods", "GET, OPTIONS")
  res$set_header("Access-Control-Allow-Headers", CORS_ALLOWED_HEADERS)
  res$set_header("Access-Control-Max-Age", "86400")
})

app$add_get(
  path = "/v1/analyses/{analysis_id}/de-summary",
  match = "regex",
  FUN = function(request, response) {
    analysis_id <- request$parameters_path$analysis_id
    validate_analysis_id(analysis_id)
    session_key <- analysis_key_from_request(request, analysis_id)
    analysis <- get_analysis(analysis_id)

    summary_path <- file.path(analysis_results_dir(analysis, "de"), "de_summary.json")
    if (!file.exists(summary_path)) {
      response$set_status_code(204L)
      return(response)
    }

    summary <- jsonlite::read_json(summary_path, simplifyVector = FALSE)
    success_response(response, summary)
  }
)

app$add_route("/v1/analyses/{analysis_id}/de-summary", match = "regex", method = "OPTIONS", FUN = function(req, res) {
  res$set_header("Access-Control-Allow-Origin", "*")
  res$set_header("Access-Control-Allow-Methods", "GET, OPTIONS")
  res$set_header("Access-Control-Allow-Headers", CORS_ALLOWED_HEADERS)
  res$set_header("Access-Control-Max-Age", "86400")
})

# upload counts to the fresh analysis
app$add_post(
  path = "/v1/analyses/{analysis_id}/upload-counts",
  match = "regex",
  FUN = function(request, response) {
    analysis_id <- request$parameters_path$analysis_id
    validate_analysis_id(analysis_id)
    # analyses other than the demo must encrypt counts with the key
    session_key <- analysis_key_from_request(request, analysis_id)
    analysis <- get_analysis(analysis_id)
    if (analysis$configured) {
      raise(error_unprocessable("Can not upload counts", "Analysis is already configured. If you want to upload new counts, please create a new analysis."))
    }

    check_disk_space(1)
    file_name <- request$headers[["X-File-Name"]] %||% "counts.csv"
    allowed_types <- c("text/csv", "text/tab-separated-values", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", "application/vnd.ms-excel")
    content_type <- tolower(request$headers[["content-type"]] %||% "")
    if (!(content_type %in% allowed_types)) {
      raise(error_bad_request(paste("Invalid Content-Type. Allowed:", paste(allowed_types, collapse = ", "))))
    }

    result <- process_count_data(request$body, analysis_id, session_key)
    counts_path <- file.path(SESSION_DATA_DIR, analysis_id, "count_matrix.fst")
    update_analysis(analysis, list(
      counts_uploaded = TRUE,
      counts_hash = hash_file(counts_path)
    ))

    success_response(response, list(
      status = "complete",
      nrows = result$nrows,
      ncolumns = result$ncolumns,
      sample_names = result$column_names
    ), status_code = 201L)
  }
)

app$add_route("/v1/analyses/{analysis_id}/upload-counts", match = "regex", method = "OPTIONS", FUN = function(req, res) {
  res$set_header("Access-Control-Allow-Origin", "*")
  res$set_header("Access-Control-Allow-Methods", "POST, OPTIONS")
  res$set_header("Access-Control-Allow-Headers", CORS_ALLOWED_HEADERS)
  res$set_header("Access-Control-Max-Age", "86400")
})

# configure analysis (eg. for DE or Explore run)
app$add_post(
  path = "/v1/analyses/{analysis_id}/config",
  match = "regex",
  FUN = function(request, response) {
    analysis_id <- request$parameters_path$analysis_id
    validate_analysis_id(analysis_id)
    session_key <- analysis_key_from_request(request, analysis_id)
    analysis <- get_analysis(analysis_id)

    if (isTRUE(analysis$running)) raise(error_conflict("Analysis already running"))

    validConfig <- validate_config(request$body, analysis_id, session_key)

    tryCatch(
      {
        config_file_path <- get_config_file_path(analysis_id)
        jsonlite::write_json(validConfig, config_file_path, pretty = TRUE, auto_unbox = TRUE)
      },
      error = function(e) {
        raise(error_internal_error("Failed to save configuration"))
      }
    )

    meta <- list(
      configured = TRUE, # this locks current analysis from new counts upload
      status = "configured",
      de_complete = FALSE,
      configured_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
    )
    update_analysis(analysis, meta)

    success_response(response, list(
      status = "configured",
      message = "Analysis configured successfully"
    ))
  }
)

app$add_route("/v1/analyses/{analysis_id}/config", match = "regex", method = "OPTIONS", FUN = function(req, res) {
  res$set_header("Access-Control-Allow-Origin", "*")
  res$set_header("Access-Control-Allow-Methods", "POST, OPTIONS")
  res$set_header("Access-Control-Allow-Headers", CORS_ALLOWED_HEADERS)
  res$set_header("Access-Control-Max-Age", "86400")
})

app$add_post(
  path = "/v1/analyses/{analysis_id}/run-de",
  match = "regex",
  FUN = function(request, response) {
    analysis_id <- request$parameters_path$analysis_id
    validate_analysis_id(analysis_id)
    session_key <- analysis_key_from_request(request, analysis_id)
    analysis <- get_analysis(analysis_id)
    if (!analysis$configured) {
      raise(error_unprocessable("Analysis is not configured", "Please configure the analysis first."))
    }
    if (analysis$running) {
      raise(error_conflict("Analysis already running"))
    }

    tryCatch(
      {
        config_file_path <- get_config_file_path(analysis_id)
        config <- jsonlite::read_json(config_file_path)
      },
      error = function(e) {
        raise(error_internal_error("Failed to load configuration"))
      }
    )

    if (is.null(config$de_parameters)) {
      raise(error_bad_request("Invalid configuration: de_parameters is required"))
    }

    # The shared results cache is demo-only. Private analyses are encrypted
    # and never compute or publish a content-hash key, so their results can
    # only ever be read from their own session directory.
    cache_params <- demo_de_cache_params(analysis, request$body, config)
    if (cache_params$hit) {
      update_analysis(analysis, cache_params$hit_analysis_meta)
      success_response(response, cache_params$hit_analysis_meta, status_code = 202L)
      return()
    }

    if (!is_demo_analysis(analysis_id)) {
      # analyses other than the demo must use encryption with the session key
      handoff_worker_analysis_key(analysis_id, analysis_key_to_hex(session_key))
    }

    task_params <- list(use_cache = cache_params$use_cache)
    if (!is.null(cache_params$cache_key)) {
      task_params$cache_key <- cache_params$cache_key
    }
    task_id <- queue_de_analysis_task(analysis_id, params = task_params)
    meta <- list(
      status = "de_queued",
      running = TRUE,
      task_id = task_id,
      gsea_complete = FALSE
    )
    update_analysis(analysis, meta)
    success_response(response, meta, status_code = 202L)
  }
)

app$add_route("/v1/analyses/{analysis_id}/run-de", match = "regex", method = "OPTIONS", FUN = function(req, res) {
  res$set_header("Access-Control-Allow-Origin", "*")
  res$set_header("Access-Control-Allow-Methods", "POST, OPTIONS")
  res$set_header("Access-Control-Allow-Headers", CORS_ALLOWED_HEADERS)
  res$set_header("Access-Control-Max-Age", "86400")
})

# Get DE analysis results, or VST expression matrix when ?vst=1
app$add_get(
  path = "/v1/analyses/{analysis_id}/de-results",
  match = "regex",
  FUN = function(request, response) {
    analysis_id <- request$parameters_path$analysis_id
    validate_analysis_id(analysis_id)
    session_key <- analysis_key_from_request(request, analysis_id)
    analysis <- get_analysis(analysis_id)

    vst_flag <- request$parameters_query[["vst"]] %||% "0"

    results_dir <- analysis_results_dir(analysis, "de")

    if (vst_flag == "1") {
      success_response(response, vst_response_payload(
        analysis_id,
        results_dir,
        session_key,
        request$parameters_query[["genes"]] %||% ""
      ))
    } else {
      de_results_path <- file.path(results_dir, "de_results.fst")

      if (analysis$de_complete == FALSE) {
        response$set_status_code(204L)
        return(response)
      }
      if (analysis$status == "de_failed") {
        raise(error_internal_error("Internal server error", "The DE analysis failed. Please check your configuration and try again. If the problem persists, contact support."))
      }
      if (!file.exists(de_results_path)) {
        raise(error_gone(
          "Results expired",
          "The results for this analysis are no longer available. Please run the analysis again."
        ))
      }

      stream_ndjson_fst(de_results_path, response, key = session_key, chunk_size = 5000, digits = NA)
      response$set_status_code(200L)
      response$print()
    }
  }
)

app$add_route("/v1/analyses/{analysis_id}/de-results", match = "regex", method = "OPTIONS", FUN = function(req, res) {
  res$set_header("Access-Control-Allow-Origin", "*")
  res$set_header("Access-Control-Allow-Methods", "GET, OPTIONS")
  res$set_header("Access-Control-Allow-Headers", CORS_ALLOWED_HEADERS)
  res$set_header("Access-Control-Max-Age", "86400")
})

app$add_post(
  path = "/v1/analyses/{analysis_id}/run-gsea",
  match = "regex",
  FUN = function(request, response) {
    analysis_id <- request$parameters_path$analysis_id
    validate_analysis_id(analysis_id)
    session_key <- analysis_key_from_request(request, analysis_id)
    analysis <- get_analysis(analysis_id)

    if (!analysis$de_complete) {
      raise(error_bad_request("DE results required", "Please run DE analysis first before running GSEA."))
    }
    if ((analysis$status == "de_failed")) {
      raise(error_internal_error("Internal server error", "The DE analysis step failed. Please check your configuration and try to run DE once again before running GSEA. If the problem persists, contact support."))
    }
    if (analysis$running) {
      raise(error_conflict("Analysis already running"))
    }

    # Parse request body
    body <- request$body
    collection_value <- body$collection
    ranking_method <- body$ranking_method %||% "stat"
    gene_of_interest <- body$gene_of_interest %||% NULL
    correlation_method <- body$correlation_method %||% "pearson"
    padj_threshold <- body$padj_threshold %||% 0.25

    if (is.null(collection_value) || collection_value == "") {
      raise(error_bad_request("Invalid request", "Collection is required."))
    }
    if (!is.numeric(padj_threshold) || is.na(padj_threshold) || padj_threshold < 0 || padj_threshold > 1) {
      raise(error_bad_request("Invalid request", "padj_threshold must be a number in [0, 1]."))
    }
    if (ranking_method == "gene_of_interest" && is.null(gene_of_interest)) {
      raise(error_bad_request("Invalid request", "gene_of_interest is required when ranking_method is 'gene_of_interest'."))
    }

    # Load config to get organism
    config_file_path <- get_config_file_path(analysis_id)
    config <- jsonlite::read_json(config_file_path)
    organism <- config$organism %||% "Mus musculus"

    which_collection <- tryCatch(
      {
        parse_collection_value(collection_value)
      },
      error = function(e) {
        raise(error_bad_request("Invalid request", e$message))
      }
    )
    if (!isTRUE(analysis$demo)) {
      handoff_worker_analysis_key(analysis_id, analysis_key_to_hex(session_key))
    }
    task_id <- queue_gsea_analysis_task(
      analysis_id = analysis_id,
      collection = which_collection$collection,
      value = which_collection$value,
      ranking_method = ranking_method,
      gene_of_interest = gene_of_interest,
      correlation_method = correlation_method,
      padj_threshold = padj_threshold
    )
    meta <- list(
      status = "gsea_queued",
      running = TRUE,
      task_id = task_id,
      gsea_complete = FALSE
    )
    update_analysis(analysis, meta)

    success_response(response, meta, status_code = 202L)
  }
)

app$add_route("/v1/analyses/{analysis_id}/run-gsea", match = "regex", method = "OPTIONS", FUN = function(req, res) {
  res$set_header("Access-Control-Allow-Origin", "*")
  res$set_header("Access-Control-Allow-Methods", "POST, OPTIONS")
  res$set_header("Access-Control-Allow-Headers", CORS_ALLOWED_HEADERS)
  res$set_header("Access-Control-Max-Age", "86400")
})

app$add_get(
  path = "/v1/analyses/{analysis_id}/gsea-results",
  match = "regex",
  FUN = function(request, response) {
    analysis_id <- request$parameters_path$analysis_id
    validate_analysis_id(analysis_id)
    session_key <- analysis_key_from_request(request, analysis_id)
    analysis <- get_analysis(analysis_id)
    gsea_results_path <- file.path(SESSION_DATA_DIR, analysis_id, "gsea_results.fst")

    if (!analysis$gsea_complete) {
      response$set_status_code(204L)
      return(response)
    }
    if (identical(
      analysis$message,
      "GSEA calculation completed, but no significant gene sets were found"
    )) {
      raise(error_not_found("No significant gene sets found", "The GSEA analysis completed,
        but no significant gene sets were found for the given parameters."))
    }
    if ((analysis$status == "gsea_failed") || !file.exists(gsea_results_path)) {
      raise(error_internal_error("Internal server error", "The GSEA analysis failed.
        Please try again."))
    }

    stream_ndjson_fst(
      gsea_results_path,
      response,
      key = session_key,
      chunk_size = 5000,
      digits = NA
    )
    response$set_status_code(200L)
    response$print()
  }
)

app$add_route("/v1/analyses/{analysis_id}/gsea-results", match = "regex", method = "OPTIONS", FUN = function(req, res) {
  res$set_header("Access-Control-Allow-Origin", "*")
  res$set_header("Access-Control-Allow-Methods", "GET, OPTIONS")
  res$set_header("Access-Control-Allow-Headers", CORS_ALLOWED_HEADERS)
  res$set_header("Access-Control-Max-Age", "86400")
})

# Get the ranked genes used by the completed GSEA run.
app$add_get(
  path = "/v1/analyses/{analysis_id}/gsea-rankings",
  match = "regex",
  FUN = function(request, response) {
    analysis_id <- request$parameters_path$analysis_id
    validate_analysis_id(analysis_id)
    session_key <- analysis_key_from_request(request, analysis_id)
    analysis <- get_analysis(analysis_id)

    if (!analysis$gsea_complete) {
      response$set_status_code(204L)
      return(response)
    }

    rankings_path <- file.path(SESSION_DATA_DIR, analysis_id, "gsea_rankings.fst")
    if (!file.exists(rankings_path)) {
      raise(error_gone(
        "GSEA rankings unavailable",
        "The ranked gene list is unavailable for this saved GSEA result. Run GSEA again to generate an enrichment plot."
      ))
    }

    rankings <- read_session_fst(rankings_path, session_key)
    success_response(response, list(
      gene_ids = I(as.character(rankings$gene_id)),
      scores = I(as.numeric(rankings$score))
    ))
  }
)

app$add_route("/v1/analyses/{analysis_id}/gsea-rankings", match = "regex", method = "OPTIONS", FUN = function(req, res) {
  res$set_header("Access-Control-Allow-Origin", "*")
  res$set_header("Access-Control-Allow-Methods", "GET, OPTIONS")
  res$set_header("Access-Control-Allow-Headers", CORS_ALLOWED_HEADERS)
  res$set_header("Access-Control-Max-Age", "86400")
})

app$add_post(
  path = "/v1/analyses/{analysis_id}/run-ora",
  match = "regex",
  FUN = function(request, response) {
    analysis_id <- request$parameters_path$analysis_id
    validate_analysis_id(analysis_id)
    session_key <- analysis_key_from_request(request, analysis_id)
    analysis <- get_analysis(analysis_id)

    if (!analysis$de_complete) {
      raise(error_bad_request("DE results required", "Please run DE analysis first before running ORA."))
    }
    if (analysis$status == "de_failed") {
      raise(error_internal_error("Internal server error", "The DE analysis failed. Please run DE analysis again before running ORA."))
    }
    if (analysis$running) {
      raise(error_conflict("Analysis already running"))
    }

    body <- request$body
    collection_value <- body$collection
    padj_threshold <- body$padj_threshold %||% 0.05
    if (is.null(collection_value) || !is.character(collection_value) ||
          length(collection_value) != 1L || !nzchar(collection_value)) {
      raise(error_bad_request("Invalid request", "Collection is required."))
    }
    if (identical(collection_value, "KEGG")) {
      raise(error_forbidden(
        "KEGG ORA is unavailable",
        "Direct KEGG ORA is currently disabled."
      ))
    }
    if (!is.numeric(padj_threshold) || length(padj_threshold) != 1L ||
          is.na(padj_threshold) || padj_threshold < 0 || padj_threshold > 1) {
      raise(error_bad_request("Invalid request", "padj_threshold must be a number in [0, 1]."))
    }

    which_collection <- tryCatch(
      parse_collection_value(collection_value),
      error = function(e) raise(error_bad_request("Invalid request", e$message))
    )

    if (!isTRUE(analysis$demo)) {
      handoff_worker_analysis_key(analysis_id, analysis_key_to_hex(session_key))
    }
    task_id <- queue_ora_analysis_task(
      analysis_id = analysis_id,
      collection = which_collection$collection,
      value = which_collection$value,
      padj_threshold = padj_threshold
    )
    meta <- list(
      status = "ora_queued",
      running = TRUE,
      task_id = task_id,
      ora_complete = FALSE,
      ora_collection = collection_value
    )
    update_analysis(analysis, meta)
    success_response(response, meta, status_code = 202L)
  }
)

app$add_route("/v1/analyses/{analysis_id}/run-ora", match = "regex", method = "OPTIONS", FUN = function(req, res) {
  res$set_header("Access-Control-Allow-Origin", "*")
  res$set_header("Access-Control-Allow-Methods", "POST, OPTIONS")
  res$set_header("Access-Control-Allow-Headers", CORS_ALLOWED_HEADERS)
  res$set_header("Access-Control-Max-Age", "86400")
})

app$add_get(
  path = "/v1/analyses/{analysis_id}/ora-results",
  match = "regex",
  FUN = function(request, response) {
    analysis_id <- request$parameters_path$analysis_id
    validate_analysis_id(analysis_id)
    session_key <- analysis_key_from_request(request, analysis_id)
    analysis <- get_analysis(analysis_id)
    ora_results_path <- file.path(SESSION_DATA_DIR, analysis_id, "ora_results.fst")

    if (!isTRUE(analysis$ora_complete)) {
      response$set_status_code(204L)
      return(response)
    }
    if (identical(analysis$message, "ORA calculation completed, but no significant gene sets were found")) {
      raise(error_not_found("No significant gene sets found", "The ORA analysis completed, but no significant gene sets were found for the given parameters."))
    }
    if (analysis$status == "ora_failed" || !file.exists(ora_results_path)) {
      raise(error_internal_error("Internal server error", "The ORA analysis failed. Please try again."))
    }

    stream_ndjson_fst(ora_results_path, response, key = session_key, chunk_size = 5000, digits = NA)
    response$set_status_code(200L)
    response$print()
  }
)

app$add_route("/v1/analyses/{analysis_id}/ora-results", match = "regex", method = "OPTIONS", FUN = function(req, res) {
  res$set_header("Access-Control-Allow-Origin", "*")
  res$set_header("Access-Control-Allow-Methods", "GET, OPTIONS")
  res$set_header("Access-Control-Allow-Headers", CORS_ALLOWED_HEADERS)
  res$set_header("Access-Control-Max-Age", "86400")
})

app$add_post(
  path = "/v1/analyses/{analysis_id}/run-correlation-ora",
  match = "regex",
  FUN = function(request, response) {
    analysis_id <- request$parameters_path$analysis_id
    validate_analysis_id(analysis_id)
    session_key <- analysis_key_from_request(request, analysis_id)
    analysis <- get_analysis(analysis_id)

    if (!isTRUE(analysis$explore_complete)) {
      raise(error_bad_request("Explore results required", "Please generate transformed data before running correlation ORA."))
    }
    if (isTRUE(analysis$running)) {
      raise(error_conflict("Analysis already running"))
    }

    body <- request$body
    correlation_task <- prepare_correlation_ora(
      body, analysis, analysis_id, session_key
    )

    if (!isTRUE(analysis$demo)) {
      handoff_worker_analysis_key(analysis_id, analysis_key_to_hex(session_key))
    }
    task_id <- queue_correlation_ora_analysis_task(
      analysis_id = analysis_id,
      collection = correlation_task$collection,
      value = correlation_task$value,
      gene_list = correlation_task$gene_list,
      universe = correlation_task$universe,
      padj_threshold = correlation_task$padj_threshold
    )
    meta <- list(
      status = "correlation_ora_queued",
      running = TRUE,
      task_id = task_id,
      correlation_ora_complete = FALSE
    )
    update_analysis(analysis, meta)
    success_response(response, meta, status_code = 202L)
  }
)

app$add_route("/v1/analyses/{analysis_id}/run-correlation-ora", match = "regex", method = "OPTIONS", FUN = function(req, res) {
  res$set_header("Access-Control-Allow-Origin", "*")
  res$set_header("Access-Control-Allow-Methods", "POST, OPTIONS")
  res$set_header("Access-Control-Allow-Headers", CORS_ALLOWED_HEADERS)
  res$set_header("Access-Control-Max-Age", "86400")
})

app$add_get(
  path = "/v1/analyses/{analysis_id}/correlation-ora-results",
  match = "regex",
  FUN = function(request, response) {
    analysis_id <- request$parameters_path$analysis_id
    validate_analysis_id(analysis_id)
    session_key <- analysis_key_from_request(request, analysis_id)
    analysis <- get_analysis(analysis_id)
    results_path <- file.path(SESSION_DATA_DIR, analysis_id, "correlation_ora_results.fst")

    if (!isTRUE(analysis$correlation_ora_complete)) {
      response$set_status_code(204L)
      return(response)
    }
    if (identical(analysis$message, "Correlation ORA completed, but no significant gene sets were found")) {
      raise(error_not_found("No significant gene sets found", "Correlation ORA completed, but no significant gene sets were found for the given parameters."))
    }
    if (identical(analysis$status, "correlation_ora_failed") || !file.exists(results_path)) {
      raise(error_internal_error("Internal server error", "Correlation ORA failed. Please try again."))
    }

    stream_ndjson_fst(results_path, response, key = session_key, chunk_size = 5000, digits = NA)
    response$set_status_code(200L)
    response$print()
  }
)

app$add_route("/v1/analyses/{analysis_id}/correlation-ora-results", match = "regex", method = "OPTIONS", FUN = function(req, res) {
  res$set_header("Access-Control-Allow-Origin", "*")
  res$set_header("Access-Control-Allow-Methods", "GET, OPTIONS")
  res$set_header("Access-Control-Allow-Headers", CORS_ALLOWED_HEADERS)
  res$set_header("Access-Control-Max-Age", "86400")
})

# data exploration (filtering + normalization + transformation).
app$add_post(
  path = "/v1/analyses/{analysis_id}/run-explore",
  match = "regex",
  FUN = function(request, response) {
    analysis_id <- request$parameters_path$analysis_id
    validate_analysis_id(analysis_id)
    session_key <- analysis_key_from_request(request, analysis_id)
    analysis <- get_analysis(analysis_id)

    if (!isTRUE(analysis$counts_uploaded)) {
      raise(error_unprocessable("Counts not uploaded", "Please upload count data first."))
    }
    if (isTRUE(analysis$running)) {
      raise(error_conflict("Analysis already running"))
    }

    params <- validate_explore_parameters(request$body)

    # Demo-only cache
    cache_params <- demo_explore_cache_params(analysis, request$body, params)
    if (cache_params$hit) {
      update_analysis(analysis, cache_params$hit_analysis_meta)
      success_response(response, cache_params$hit_analysis_meta, status_code = 202L)
      return()
    }

    if (!is_demo_analysis(analysis_id)) {
      handoff_worker_analysis_key(analysis_id, analysis_key_to_hex(session_key))
    }
    task_params <- params
    task_params$use_cache <- cache_params$use_cache
    if (!is.null(cache_params$cache_key)) {
      task_params$cache_key <- cache_params$cache_key
    }
    task_id <- queue_explore_analysis_task(analysis_id, task_params)
    meta <- list(
      status = "explore_queued",
      running = TRUE,
      task_id = task_id,
      explore_complete = FALSE,
      organism = params$organism
    )
    update_analysis(analysis, meta)
    success_response(response, meta, status_code = 202L)
  }
)

app$add_route("/v1/analyses/{analysis_id}/run-explore", match = "regex", method = "OPTIONS", FUN = function(req, res) {
  res$set_header("Access-Control-Allow-Origin", "*")
  res$set_header("Access-Control-Allow-Methods", "POST, OPTIONS")
  res$set_header("Access-Control-Allow-Headers", CORS_ALLOWED_HEADERS)
  res$set_header("Access-Control-Max-Age", "86400")
})

# Transformed expression matrix (NDJSON: header line with
# sample names, then one gene row per line)
app$add_get(
  path = "/v1/analyses/{analysis_id}/explore-results",
  match = "regex",
  FUN = function(request, response) {
    analysis_id <- request$parameters_path$analysis_id
    validate_analysis_id(analysis_id)
    session_key <- analysis_key_from_request(request, analysis_id)
    analysis <- get_analysis(analysis_id)

    if (!isTRUE(analysis$explore_complete)) {
      response$set_status_code(204L)
      return(response)
    }

    results_path <- file.path(analysis_results_dir(analysis, "explore"), "explore_results.fst")
    if (identical(analysis$status, "explore_failed")) {
      raise(error_internal_error(
        "Internal server error",
        "The exploration failed. Please run the exploration again. If the problem persists, contact support."
      ))
    }
    if (!file.exists(results_path)) {
      raise(error_gone(
        "Results expired",
        "The exploration results are no longer available. Please run the exploration again."
      ))
    }

    annotation_cols <- c("symbol", "geneInfo", "entrez", "entrezNcbi")
    col_names <- metadata_session_fst(results_path, session_key)$columnNames
    sample_cols <- setdiff(col_names, c("gene_id", annotation_cols))

    # Table header with sample names
    prelude <- as.character(jsonlite::toJSON(
      list(samples = I(as.character(sample_cols))),
      auto_unbox = TRUE, null = "null", na = "null"
    ))

    row_to_json_fn <- function(row) {
      out <- list(gene_id = row$gene_id)
      for (col in annotation_cols) {
        # Only emit columns that exist: jsonlite serializes a missing
        # (NULL) element as {}
        if (col %in% names(row)) out[[col]] <- row[[col]]
      }
      out$values <- I(unname(as.numeric(unlist(row[sample_cols], use.names = FALSE))))
      jsonlite::toJSON(out, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
    }

    stream_ndjson_fst(
      results_path, response,
      key = session_key,
      chunk_size = 5000, digits = NA,
      sci_notation_cols = NULL,
      prelude_lines = prelude,
      row_to_json_fn = row_to_json_fn
    )
    response$set_status_code(200L)
    response$print()
  }
)

app$add_route("/v1/analyses/{analysis_id}/explore-results", match = "regex", method = "OPTIONS", FUN = function(req, res) {
  res$set_header("Access-Control-Allow-Origin", "*")
  res$set_header("Access-Control-Allow-Methods", "GET, OPTIONS")
  res$set_header("Access-Control-Allow-Headers", CORS_ALLOWED_HEADERS)
  res$set_header("Access-Control-Max-Age", "86400")
})

app$add_get(
  path = "/v1/analyses/{analysis_id}/explore-summary",
  match = "regex",
  FUN = function(request, response) {
    analysis_id <- request$parameters_path$analysis_id
    validate_analysis_id(analysis_id)
    session_key <- analysis_key_from_request(request, analysis_id)
    analysis <- get_analysis(analysis_id)

    summary_path <- file.path(analysis_results_dir(analysis, "explore"), "explore_summary.json")
    if (!file.exists(summary_path)) {
      response$set_status_code(204L)
      return(response)
    }

    summary <- jsonlite::read_json(summary_path, simplifyVector = FALSE)
    success_response(response, summary)
  }
)

app$add_route("/v1/analyses/{analysis_id}/explore-summary", match = "regex", method = "OPTIONS", FUN = function(req, res) {
  res$set_header("Access-Control-Allow-Origin", "*")
  res$set_header("Access-Control-Allow-Methods", "GET, OPTIONS")
  res$set_header("Access-Control-Allow-Headers", CORS_ALLOWED_HEADERS)
  res$set_header("Access-Control-Max-Age", "86400")
})
