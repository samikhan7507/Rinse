# Structured logging to console and a rotating file (storage/logs/rinse.log).
LOG_LEVELS <- c(DEBUG = 1, INFO = 2, WARN = 3, ERROR = 4)

log_file <- function() file.path(dirname(CFG$db_path), "logs", "rinse.log")

rotate_log <- function(f, max_bytes = 2 * 1024^2, keep = 3) {
  if (!file.exists(f) || file.size(f) < max_bytes) return(invisible())
  for (i in keep:1) { a <- paste0(f, ".", i); if (file.exists(a)) { if (i == keep) file.remove(a) else file.rename(a, paste0(f, ".", i + 1)) } }
  file.rename(f, paste0(f, ".1"))
}

vlog <- function(level, msg, ...) {
  lv <- toupper(level)
  if (LOG_LEVELS[[lv]] < LOG_LEVELS[[toupper(CFG$log_level)]]) return(invisible())
  line <- sprintf("%s [%s] %s", format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), lv, sprintf(msg, ...))
  message(line)
  tryCatch({ f <- log_file(); dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE); rotate_log(f); cat(line, "\n", file = f, append = TRUE, sep = "") }, error = function(e) NULL)
  invisible(line)
}
