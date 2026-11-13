#!/usr/bin/env Rscript

source("api/helpers/build_info.R")

valid_env <- c(
  GENLYZER_SOURCE_REVISION = paste(rep("a", 40L), collapse = ""),
  GENLYZER_ARTIFACT_DIGEST = paste0("sha256:", paste(rep("b", 64L), collapse = "")),
  GENLYZER_BUILD_PROVENANCE_URL = "https://github.com/example/repo/attestations/123",
  GENLYZER_BUILD_TIMESTAMP = "2026-11-08T12:00:00Z"
)

complete <- get_build_info(valid_env)
stopifnot(
  identical(complete$metadata_status, "complete"),
  identical(complete$source_revision, unname(valid_env[["GENLYZER_SOURCE_REVISION"]])),
  identical(complete$artifact_digest, unname(valid_env[["GENLYZER_ARTIFACT_DIGEST"]])),
  identical(complete$provenance_url, unname(valid_env[["GENLYZER_BUILD_PROVENANCE_URL"]])),
  identical(complete$built_at, unname(valid_env[["GENLYZER_BUILD_TIMESTAMP"]])),
  identical(complete$verification, "informational_only")
)

incomplete <- get_build_info(character())
stopifnot(
  identical(incomplete$metadata_status, "incomplete"),
  is.null(incomplete$source_revision),
  is.null(incomplete$artifact_digest),
  is.null(incomplete$provenance_url)
)

malformed <- get_build_info(c(
  GENLYZER_SOURCE_REVISION = "main",
  GENLYZER_ARTIFACT_DIGEST = "latest",
  GENLYZER_BUILD_PROVENANCE_URL = "http://example.com/build",
  GENLYZER_BUILD_TIMESTAMP = "yesterday"
))
stopifnot(
  identical(malformed$metadata_status, "incomplete"),
  is.null(malformed$source_revision),
  is.null(malformed$artifact_digest),
  is.null(malformed$provenance_url),
  is.null(malformed$built_at)
)

cat("PASS: build metadata validation and incomplete defaults\n")
