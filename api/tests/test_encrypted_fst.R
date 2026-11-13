#!/usr/bin/env Rscript

if (!requireNamespace("openssl", quietly = TRUE) ||
    !requireNamespace("digest", quietly = TRUE) ||
    !requireNamespace("fst", quietly = TRUE)) {
  stop("Encrypted FST tests require openssl, digest, and fst", call. = FALSE)
}

source("api/helpers/encrypted_fst.R")

test_root <- tempfile("genlyzer-encryption-test-")
SESSION_DECRYPTED_TMP_DIR <- file.path(test_root, "ram-staging")
session_dir <- file.path(test_root, "sessions", "analysis-1")
dir.create(session_dir, recursive = TRUE)
key <- as.raw(seq_len(32L))
wrong_key <- as.raw(rep(99L, 32L))
sample <- data.frame(
  gene_id = c("ENSMUSG00000000001", "ENSMUSG00000000002"),
  count = c(123L, 456L),
  note = c("SENSITIVE_SESSION_SENTINEL", "sample-2"),
  stringsAsFactors = FALSE
)
path <- file.path(session_dir, "count_matrix.fst")
SESSION_DATA_DIR <- dirname(session_dir)

write_session_fst(sample, path, key, compress = 0L)
stored <- readBin(path, what = "raw", n = file.info(path)$size)
stopifnot(identical(stored[seq_along(SESSION_FST_MAGIC)], SESSION_FST_MAGIC))
sentinel <- charToRaw("SENSITIVE_SESSION_SENTINEL")
contains_bytes <- function(haystack, needle) {
  if (length(haystack) < length(needle)) return(FALSE)
  any(vapply(
    seq_len(length(haystack) - length(needle) + 1L),
    function(start) identical(haystack[start:(start + length(needle) - 1L)], needle),
    logical(1)
  ))
}
stopifnot(!contains_bytes(stored, sentinel))
stopifnot(identical(read_session_fst(path, key), sample))
stopifnot(metadata_session_fst(path, key)$nrOfRows == nrow(sample))
stopifnot(inherits(try(read_session_fst(path, wrong_key), silent = TRUE), "try-error"))

source("api/helpers/file_handling.R")
source("api/helpers/validation.R")
configured <- validate_config(
  list(
    organism = "Mus musculus",
    name_column = "gene_id",
    sample_columns = "count"
  ),
  "analysis-1",
  key
)
stopifnot(identical(configured$name_column, "gene_id"))

tampered <- stored
tampered[length(tampered)] <- as.raw(bitwXor(as.integer(tampered[length(tampered)]), 1L))
tampered_path <- file.path(session_dir, "tampered.fst")
writeBin(tampered, tampered_path)
stopifnot(inherits(try(read_session_fst(tampered_path, key), silent = TRUE), "try-error"))

plain_files <- list.files(SESSION_DECRYPTED_TMP_DIR, all.files = TRUE, no.. = TRUE)
stopifnot(length(plain_files) == 0L)

cat("PASS: encrypted FST round-trip, ciphertext-at-rest, and authentication checks\n")
