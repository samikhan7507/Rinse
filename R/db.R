# SQLite storage with versioned migrations (Unit III: databases).
# Tables: users, sessions (remember-me), otps, runs, model_runs, login_events, user_sessions, audit.
ago_ts <- function(seconds) format(Sys.time() - seconds, "%Y-%m-%d %H:%M:%S")

db_connect <- function() {
  dir.create(dirname(CFG$db_path), showWarnings = FALSE, recursive = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), CFG$db_path)
  DBI::dbExecute(con, "PRAGMA busy_timeout = 8000")   # wait instead of failing when another session writes
  invisible(DBI::dbGetQuery(con, "PRAGMA journal_mode = WAL"))   # readers do not block the writer
  con
}
with_db <- function(fn) { con <- db_connect(); on.exit(DBI::dbDisconnect(con)); fn(con) }
q_get  <- function(sql, params = list()) with_db(function(con) if (length(params)) DBI::dbGetQuery(con, sql, params = unname(params)) else DBI::dbGetQuery(con, sql))
q_exec <- function(sql, params = list()) with_db(function(con) if (length(params)) DBI::dbExecute(con, sql, params = unname(params)) else DBI::dbExecute(con, sql))

MIGRATIONS <- list(
  `1` = c(
    "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY AUTOINCREMENT, email TEXT UNIQUE, phone TEXT UNIQUE, name TEXT, pw_hash TEXT, role TEXT DEFAULT 'user', provider TEXT DEFAULT 'email', active INTEGER DEFAULT 1, created_at TEXT, last_login TEXT, fail_count INTEGER DEFAULT 0, locked_until TEXT)",
    "CREATE TABLE IF NOT EXISTS otps (id INTEGER PRIMARY KEY AUTOINCREMENT, phone TEXT, code_hash TEXT, expires_at TEXT, attempts INTEGER DEFAULT 0, created_at TEXT)",
    "CREATE TABLE IF NOT EXISTS runs (id INTEGER PRIMARY KEY AUTOINCREMENT, user_id INTEGER, filename TEXT, n_rows INTEGER, n_cols INTEGER, target TEXT, score_before INTEGER, score_after INTEGER, grade TEXT, created_at TEXT)",
    "CREATE TABLE IF NOT EXISTS sessions (token_hash TEXT PRIMARY KEY, user_id INTEGER, expires_at TEXT)",
    "CREATE TABLE IF NOT EXISTS audit (id INTEGER PRIMARY KEY AUTOINCREMENT, ts TEXT, user_id INTEGER, event TEXT, detail TEXT)"),
  `2` = c(
    "ALTER TABLE users ADD COLUMN login_count INTEGER DEFAULT 0",
    "ALTER TABLE users ADD COLUMN theme TEXT DEFAULT 'light'",
    "ALTER TABLE users ADD COLUMN must_change INTEGER DEFAULT 0",
    "CREATE TABLE IF NOT EXISTS login_events (id INTEGER PRIMARY KEY AUTOINCREMENT, ts TEXT, user_id INTEGER, identifier TEXT, method TEXT, success INTEGER, reason TEXT, ip TEXT, device TEXT, user_agent TEXT)",
    "CREATE TABLE IF NOT EXISTS user_sessions (id TEXT PRIMARY KEY, user_id INTEGER, started TEXT, last_seen TEXT, ended TEXT, ip TEXT, device TEXT, page TEXT)",
    "CREATE TABLE IF NOT EXISTS model_runs (id INTEGER PRIMARY KEY AUTOINCREMENT, run_id INTEGER, user_id INTEGER, filename TEXT, model TEXT, task TEXT, metric TEXT, value REAL, seconds REAL, created_at TEXT)",
    "CREATE INDEX IF NOT EXISTS idx_login_ts ON login_events(ts)",
    "CREATE INDEX IF NOT EXISTS idx_sess_seen ON user_sessions(last_seen)",
    "CREATE INDEX IF NOT EXISTS idx_runs_user ON runs(user_id)"))

db_version <- function() {
  with_db(function(con) {
    DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS schema_version (v INTEGER)")
    v <- DBI::dbGetQuery(con, "SELECT MAX(v) AS v FROM schema_version")$v
    if (is.na(v)) 0L else as.integer(v)
  })
}

db_init <- function() {
  cur <- db_version()
  for (v in sort(as.integer(names(MIGRATIONS)))) if (v > cur) {
    with_db(function(con) {
      DBI::dbWithTransaction(con, { for (s in MIGRATIONS[[as.character(v)]]) DBI::dbExecute(con, s); DBI::dbExecute(con, "INSERT INTO schema_version (v) VALUES (?)", params = list(v)) })
    })
    vlog("INFO", "Database migrated to version %d", v)
  }
  seed_admin()
  invisible(TRUE)
}

seed_admin <- function() {
  ex <- q_get("SELECT id FROM users WHERE email = ?", list(CFG$admin_email))
  if (nrow(ex) == 0) {
    q_exec("INSERT INTO users (email, name, pw_hash, role, provider, created_at) VALUES (?,?,?,?,?,?)",
           list(CFG$admin_email, CFG$admin_name, hash_pw(CFG$admin_password), "admin", "email", now_ts()))
    vlog("INFO", "Administrator account created for %s", CFG$admin_email)
    if (isTRUE(ADMIN_PW_GENERATED)) message(sprintf("First start: administrator %s, generated password %s  (change it in Settings, or set RINSE_ADMIN_PASSWORD)", CFG$admin_email, CFG$admin_password))
  } else q_exec("UPDATE users SET role = 'admin' WHERE email = ?", list(CFG$admin_email))
}

audit <- function(user_id, event, detail = "") {
  tryCatch(q_exec("INSERT INTO audit (ts, user_id, event, detail) VALUES (?,?,?,?)", list(now_ts(), if (is.null(user_id)) NA_integer_ else user_id, event, substr(detail, 1, 300))), error = function(e) NULL)
}

save_run <- function(user_id, filename, rows, cols, target, before, after, grade) {
  q_exec("INSERT INTO runs (user_id, filename, n_rows, n_cols, target, score_before, score_after, grade, created_at) VALUES (?,?,?,?,?,?,?,?,?)",
         list(user_id, filename, rows, cols, ifelse(nzchar(target), target, NA_character_), before, after, grade, now_ts()))
  q_get("SELECT MAX(id) AS id FROM runs WHERE user_id = ?", list(user_id))$id
}
user_runs <- function(user_id, n = 50) q_get("SELECT id, filename, n_rows, n_cols, target, score_before, score_after, grade, created_at FROM runs WHERE user_id = ? ORDER BY id DESC LIMIT ?", list(user_id, n))

save_model_runs <- function(run_id, user_id, filename, res, task) {
  if (is.null(res) || !nrow(res)) return(invisible())
  for (i in seq_len(nrow(res)))
    q_exec("INSERT INTO model_runs (run_id, user_id, filename, model, task, metric, value, seconds, created_at) VALUES (?,?,?,?,?,?,?,?,?)",
           list(if (is.null(run_id)) NA_integer_ else run_id, user_id, filename, res$model[i], task, res$main_metric[i], res$main[i], res$seconds[i], now_ts()))
}

# ---- Database browser helpers (admin) ----
HIDDEN_COLS <- c("pw_hash", "code_hash", "token_hash")
db_tables <- function() setdiff(with_db(function(con) DBI::dbListTables(con)), c("sqlite_sequence", "schema_version", "otps", "sessions"))
db_read_table <- function(name, limit = 5000) {
  if (!name %in% db_tables()) stop("Unknown table.", call. = FALSE)
  d <- q_get(sprintf("SELECT * FROM `%s` ORDER BY rowid DESC LIMIT %d", name, as.integer(limit)))
  d[, setdiff(names(d), HIDDEN_COLS), drop = FALSE]
}
db_stats <- function() {
  tb <- db_tables()
  counts <- vapply(tb, function(t) q_get(sprintf("SELECT COUNT(*) n FROM `%s`", t))$n, numeric(1))
  list(size_kb = round(file.size(CFG$db_path) / 1024, 1), version = db_version(), tables = data.frame(Table = tb, Rows = as.integer(counts), stringsAsFactors = FALSE))
}
db_backup <- function(dest = NULL) {
  if (is.null(dest)) dest <- file.path(dirname(CFG$db_path), "backups", paste0("rinse_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".sqlite"))
  dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
  src <- db_connect(); on.exit(DBI::dbDisconnect(src))
  dst <- DBI::dbConnect(RSQLite::SQLite(), dest)
  RSQLite::sqliteCopyDatabase(src, dst); DBI::dbDisconnect(dst)
  vlog("INFO", "Database backup written to %s", dest)
  dest
}
db_healthy <- function() isTRUE(tryCatch(q_get("SELECT 1 AS ok")$ok == 1, error = function(e) FALSE))
