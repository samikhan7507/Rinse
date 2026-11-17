# Import data from CSV, TSV, Excel, JSON, SQLite and web URLs (Unit III).
read_any <- function(path, name = basename(path), sheet = 1) {
  ext <- tolower(tools::file_ext(name))
  df <- switch(ext,
    csv  = utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = character(0),
                           colClasses = "character", encoding = "UTF-8"),
    tsv  = utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = character(0), colClasses = "character"),
    txt  = utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = character(0), colClasses = "character"),
    xlsx = as.data.frame(readxl::read_excel(path, sheet = sheet, col_types = "text")),
    xls  = as.data.frame(readxl::read_excel(path, sheet = sheet, col_types = "text")),
    json = {
      x <- jsonlite::fromJSON(path, flatten = TRUE)
      # Web APIs often wrap the table, e.g. {"status": "ok", "data": [ ... ]}: use the first table inside.
      if (is.list(x) && !is.data.frame(x)) { tabs <- Filter(is.data.frame, x); if (length(tabs)) x <- tabs[[1]] }
      as.data.frame(x)
    },
    stop("Unsupported file type '.", ext, "'. Use CSV, TSV, Excel, JSON or SQLite.", call. = FALSE))
  validate_df(df)
}

validate_df <- function(df) {
  if (!is.data.frame(df) || ncol(df) == 0) stop("The file has no columns.", call. = FALSE)
  if (nrow(df) < 5) stop("The file needs at least 5 rows to be analysed.", call. = FALSE)
  if (nrow(df) > 200000) stop("The file has more than 200,000 rows. Upload a smaller sample.", call. = FALSE)
  nm <- trimws(sub("^(\ufeff|\u00ef\u00bb\u00bf)", "", names(df)))
  nm[is.na(nm) | nm == ""] <- paste0("column_", seq_along(nm))[is.na(nm) | nm == ""]
  names(df) <- make.unique(nm, sep = "_")
  df[] <- lapply(df, function(x) {
    if (is.list(x)) x <- vapply(x, function(v) {
      if (is.null(v) || length(v)==0) "" else paste(v, collapse = ",")
    }, "")
    as.character(x)
  })
  df
}

excel_sheets_safe <- function(path) tryCatch(readxl::excel_sheets(path), error = function(e) character(0))

sqlite_tables <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path); on.exit(DBI::dbDisconnect(con))
  DBI::dbListTables(con)
}

read_sqlite <- function(path, table) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path); on.exit(DBI::dbDisconnect(con))
  if (!table %in% DBI::dbListTables(con)) stop("Table not found.", call. = FALSE)
  df <- DBI::dbGetQuery(con, sprintf("SELECT * FROM `%s` LIMIT 200000", gsub("`", "", table)))
  validate_df(df)
}

read_url <- function(url) {
  url <- trimws(url)
  if (!grepl("^https://", url)) stop("Only secure https:// links are allowed.", call. = FALSE)
  if (grepl("^https://(localhost|127\\.|10\\.|192\\.168\\.|169\\.254\\.|0\\.)", url))
    stop("Private addresses are not allowed.", call. = FALSE)
  tmp <- tempfile()
  r <- tryCatch(httr::GET(url, httr::timeout(20), httr::write_disk(tmp, overwrite = TRUE)),
                error = function(e) stop("Could not reach that link. Check the address and your internet connection.", call. = FALSE))
  if (httr::status_code(r) >= 400) stop("The link returned an error (", httr::status_code(r), ").", call. = FALSE)
  if (file.size(tmp) > CFG$max_upload_mb * 1024^2) stop("The file at that link is too large.", call. = FALSE)
  # A web API link rarely ends in ".json", so decide from what the server says and from the first characters.
  ctype <- tolower(as.character(httr::headers(r)[["content-type"]] %||% ""))
  first <- tryCatch(trimws(paste(readLines(tmp, n = 2, warn = FALSE, encoding = "UTF-8"), collapse = " ")), error = function(e) "")
  is_json <- grepl("json", ctype) || grepl("\\.json($|\\?)", url) || grepl("^[\\[{]", first)
  final <- paste0(tmp, if (is_json) ".json" else ".csv")
  file.rename(tmp, final)
  read_any(final, basename(final))
}
