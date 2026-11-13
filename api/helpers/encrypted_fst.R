SESSION_DECRYPTED_TMP_DIR <- Sys.getenv(
  "SESSION_DECRYPTED_TMP_DIR",
  unset = if (dir.exists("/dev/shm")) "/dev/shm/genlyzer" else ""
)

SESSION_FST_MAGIC <- charToRaw("GENFST01")
SESSION_FST_IV_LENGTH <- 16L
SESSION_FST_MAC_LENGTH <- 32L

analysis_key_from_header <- function(value, required = TRUE) {
  if (is.null(value) || length(value) != 1L || !is.character(value) || !nzchar(value)) {
    if (required) stop("X-Analysis-Key header is required", call. = FALSE)
    return(NULL)
  }
  if (!grepl("^[0-9a-fA-F]{64}$", value)) {
    stop("X-Analysis-Key must contain a 256-bit hexadecimal key", call. = FALSE)
  }
  as.raw(strtoi(substring(value, seq(1L, 63L, by = 2L), seq(2L, 64L, by = 2L)), 16L))
}

analysis_key_from_request <- function(request, analysis_id) {
  if (is_demo_analysis(analysis_id)) return(NULL)
  key <- tryCatch(
    analysis_key_from_header(request$headers[["x-analysis-key"]]),
    error = function(e) NULL
  )
  if (is.null(key)) {
    raise(error_forbidden(
      "Analysis key required",
      "This analysis requires its browser-held encryption key."
    ))
  }
  key
}

analysis_key_to_hex <- function(key) {
  paste(sprintf("%02x", as.integer(key)), collapse = "")
}

ensure_session_tmp_dir <- function() {
  if (!nzchar(SESSION_DECRYPTED_TMP_DIR)) {
    stop(
      paste(
        "Set SESSION_DECRYPTED_TMP_DIR to a RAM-backed directory before using encrypted sessions."
      ),
      call. = FALSE
    )
  }
  if (!dir.exists(SESSION_DECRYPTED_TMP_DIR)) {
    dir.create(SESSION_DECRYPTED_TMP_DIR, recursive = TRUE, mode = "0700")
  }
  if (!dir.exists(SESSION_DECRYPTED_TMP_DIR) ||
        file.access(SESSION_DECRYPTED_TMP_DIR, 2L) != 0L) {
    stop("Encrypted session temporary directory is unavailable", call. = FALSE)
  }
  Sys.chmod(SESSION_DECRYPTED_TMP_DIR, mode = "0700")
  invisible(SESSION_DECRYPTED_TMP_DIR)
}

initialize_session_tmp_dir <- function() {
  ensure_session_tmp_dir()
  stale_files <- list.files(
    SESSION_DECRYPTED_TMP_DIR,
    pattern = "^genlyzer-(plain|fst|key)-",
    full.names = TRUE
  )
  if (length(stale_files) > 0L) {
    unlink(stale_files, force = TRUE)
  }
  invisible(SESSION_DECRYPTED_TMP_DIR)
}

session_fst_mac_key <- function(key) {
  digest::hmac(key, "genlyzer/fst/mac/v1", algo = "sha256", raw = TRUE)
}

session_fst_encryption_key <- function(key) {
  digest::hmac(key, "genlyzer/fst/encryption/v1", algo = "sha256", raw = TRUE)
}

session_fst_mac <- function(key, payload) {
  digest::hmac(session_fst_mac_key(key), payload, algo = "sha256", raw = TRUE)
}

raw_constant_time_equal <- function(left, right) {
  if (length(left) != length(right)) return(FALSE)
  difference <- 0L
  for (i in seq_along(left)) {
    difference <- bitwOr(
      difference,
      bitwXor(as.integer(left[[i]]), as.integer(right[[i]]))
    )
  }
  difference == 0L
}

encrypt_session_fst_bytes <- function(plain, key) {
  if (!is.raw(key) || length(key) != 32L) stop("Invalid session encryption key", call. = FALSE)
  iv <- openssl::rand_bytes(SESSION_FST_IV_LENGTH)
  ciphertext <- openssl::aes_ctr_encrypt(
    plain,
    session_fst_encryption_key(key),
    iv = iv
  )
  authenticated <- c(SESSION_FST_MAGIC, iv, ciphertext)
  c(authenticated[seq_len(length(SESSION_FST_MAGIC) + SESSION_FST_IV_LENGTH)],
    session_fst_mac(key, authenticated),
    ciphertext)
}

decrypt_session_fst_bytes <- function(envelope, key) {
  header_length <- length(SESSION_FST_MAGIC) + SESSION_FST_IV_LENGTH
  minimum_length <- header_length + SESSION_FST_MAC_LENGTH
  if (!is.raw(key) || length(key) != 32L ||
        !is.raw(envelope) || length(envelope) < minimum_length ||
        !identical(envelope[seq_along(SESSION_FST_MAGIC)], SESSION_FST_MAGIC)) {
    stop("Invalid encrypted session file", call. = FALSE)
  }

  header <- envelope[seq_len(header_length)]
  mac_start <- header_length + 1L
  mac_end <- header_length + SESSION_FST_MAC_LENGTH
  supplied_mac <- envelope[mac_start:mac_end]
  ciphertext <- if (length(envelope) == mac_end) raw(0) else envelope[(mac_end + 1L):length(envelope)]
  authenticated <- c(header, ciphertext)
  expected_mac <- session_fst_mac(key, authenticated)
  if (!raw_constant_time_equal(supplied_mac, expected_mac)) {
    stop("Encrypted session file authentication failed", call. = FALSE)
  }

  iv <- envelope[(length(SESSION_FST_MAGIC) + 1L):header_length]
  openssl::aes_ctr_decrypt(ciphertext, session_fst_encryption_key(key), iv = iv)
}

with_plain_session_fst <- function(path, key, fn) {
  if (is.null(key)) {
    return(fn(path))
  }
  if (!file.exists(path)) stop("FST file not found", call. = FALSE)
  ensure_session_tmp_dir()
  plain_path <- tempfile(pattern = "genlyzer-plain-", tmpdir = SESSION_DECRYPTED_TMP_DIR)
  on.exit(unlink(plain_path, force = TRUE), add = TRUE)

  encrypted <- readBin(path, what = "raw", n = file.info(path)$size)
  plain <- decrypt_session_fst_bytes(encrypted, key)
  writeBin(plain, plain_path)
  Sys.chmod(plain_path, mode = "0600")
  rm(encrypted, plain)
  fn(plain_path)
}

write_session_fst <- function(data, path, key = NULL, ...) {
  if (is.null(key)) {
    return(fst::write_fst(data, path, ...))
  }
  if (!is.raw(key) || length(key) != 32L) stop("Invalid session encryption key", call. = FALSE)
  ensure_session_tmp_dir()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  plain_path <- tempfile(pattern = "genlyzer-fst-", tmpdir = SESSION_DECRYPTED_TMP_DIR)
  encrypted_path <- tempfile(pattern = ".encrypted-", tmpdir = dirname(path))
  on.exit(unlink(c(plain_path, encrypted_path), force = TRUE), add = TRUE)

  fst::write_fst(data, plain_path, ...)
  Sys.chmod(plain_path, mode = "0600")
  plain <- readBin(plain_path, what = "raw", n = file.info(plain_path)$size)
  encrypted <- encrypt_session_fst_bytes(plain, key)
  writeBin(encrypted, encrypted_path)
  Sys.chmod(encrypted_path, mode = "0600")
  rm(plain, encrypted)

  if (!file.rename(encrypted_path, path)) {
    stop("Failed to save encrypted session data", call. = FALSE)
  }
  invisible(path)
}

read_session_fst <- function(path, key = NULL, ...) {
  with_plain_session_fst(path, key, function(plain_path) {
    fst::read_fst(plain_path, ...)
  })
}

metadata_session_fst <- function(path, key = NULL, ...) {
  with_plain_session_fst(path, key, function(plain_path) {
    fst::metadata_fst(plain_path, ...)
  })
}
