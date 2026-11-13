# Database / Software Version Helper Functions
# Reads annotation database version tables and returns a structured list.

source("api/config/database_config.R")

first_value <- function(df, col, fallback = "Unknown") {
  if (is.null(df) || nrow(df) == 0 || !(col %in% names(df))) return(fallback)
  v <- df[[col]][1]
  if (is.null(v) || is.na(v)) fallback else as.character(v)
}

# Read one organism's version tables and return a list of human-readable lines
get_organism_versions_list <- function(db_path) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  tables <- tryCatch(DBI::dbListTables(conn), error = function(e) character())
  has <- function(name) name %in% tables

  query_version <- function(table, default_df) {
    if (!has(table)) return(NULL)
    tryCatch({
      sql <- if (table %in% c("msigdb_version", "msigdb_human_version"))
        paste0("SELECT * FROM ", table, " ORDER BY id DESC LIMIT 1") else
        paste0("SELECT * FROM ", table)
      DBI::dbGetQuery(conn, sql)
    }, error = function(e) default_df)
  }

  ensembl_gtf <- query_version("ensembl_gtf_version", data.frame(version = NA, date = NA))
  msig_mouse <- query_version("msigdb_version", data.frame(version_name = NA, version_date = NA))
  msig_human <- query_version("msigdb_human_version", data.frame(version_name = NA, version_date = NA))
  goa <- query_version("goa_human_version", data.frame(version = NA, date = NA))
  go <- query_version("go_version", data.frame(version = NA, date = NA))
  mgi <- query_version("mgi_version", data.frame(version = NA, date = NA))
  hgnc_mapping <- query_version(
    "hgnc_mapping_version", data.frame(version = NA, date = NA)
  )
  mgi_mapping <- query_version(
    "mgi_ensembl_mapping_version", data.frame(version = NA, date = NA)
  )

  lines <- character()
  add_line <- function(prefix, df, version_col, date_col, suffix = "") {
    if (is.null(df) || nrow(df) == 0) return(invisible(NULL))
    version <- first_value(df, version_col)
    date <- first_value(df, date_col)
    lines <<- c(lines, paste0(prefix, version, " from ", date, suffix))
  }

  add_line("Ensembl GTF: ", ensembl_gtf, "version", "date", " | Apache 2.0")
  add_line("MSigDB: ", msig_mouse, "version_name", "version_date", " | CC-BY-4.0")
  add_line("MSigDB: ", msig_human, "version_name", "version_date", " | CC-BY-4.0")
  add_line("GOA: ", goa, "version", "date", " (GOA)")
  add_line("GO terms: ", go, "version", "date", " (GO.db)")
  add_line("MGI: ", mgi, "version", "date", " | CC BY 4.0")
  add_line("HGNC mapping: ", hgnc_mapping, "version", "date")
  add_line("MGI-Ensembl mapping: ", mgi_mapping, "version", "date", " | CC BY 4.0")

  lines
}

# Return all versions grouped by organism
get_versions_all <- function() {
  mouse_lines <- tryCatch(
    get_organism_versions_list(DB_MOUSE_PATH),
    error = function(e) {
      message("Failed to read mouse version tables")
      character()
    }
  )
  human_lines <- tryCatch(
    get_organism_versions_list(DB_HUMAN_PATH),
    error = function(e) {
      message("Failed to read human version tables")
      character()
    }
  )

  list(
    mus_musculus = mouse_lines,
    homo_sapiens = human_lines
  )
}
