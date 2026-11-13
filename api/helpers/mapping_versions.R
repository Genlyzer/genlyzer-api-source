mappingFileVersion <- function(con, table_name, source, file_path, url) {
  if (!file.exists(file_path) || is.na(file.info(file_path)$size) || file.info(file_path)$size <= 0) {
    stop("Cannot record mapping version for missing or empty file: ", file_path)
  }

  cache_path <- paste0(file_path, ".http-cache.rds")
  downloaded_at <- NULL
  if (file.exists(cache_path)) {
    cache <- readRDS(cache_path)
    if (is.list(cache) && is.character(cache$downloaded_at) &&
        length(cache$downloaded_at) == 1L && !is.na(cache$downloaded_at) &&
        nzchar(cache$downloaded_at)) {
      downloaded_at <- as.POSIXct(cache$downloaded_at, tz = "UTC")
    }
  }
  if (is.null(downloaded_at) || is.na(downloaded_at)) {
    downloaded_at <- file.info(file_path)$mtime
  }

  version <- data.frame(
    source = source,
    version = basename(file_path),
    date = format(downloaded_at, "%Y-%m-%d", tz = "UTC"),
    url = url,
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, table_name, version, overwrite = TRUE)
  message(sprintf("Recorded %s file date: %s", source, version$date))
  invisible(version)
}
