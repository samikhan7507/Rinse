# Rinse configuration: everything configurable lives here or in environment variables.
APP_NAME    <- "Rinse"
APP_TAGLINE <- "Check, clean and prepare your data for machine learning"
APP_VERSION <- "3.0.0"
AUTHOR <- list(name = "Md Sami Ahmad", email = "samikhan57705@gmail.com",
               linkedin = "https://www.linkedin.com/in/samikhan07",
               github = "https://github.com/samikhan7507")

# Reads an environment variable. RINSE_* is the current prefix. The names used by earlier versions
# (SIFT_*, and before that VYREL_*) are still honoured so existing deployments keep working.
env <- function(key, default = "") {
  for (k in unique(c(key, sub("^RINSE_", "SIFT_", key), sub("^RINSE_", "VYREL_", key)))) {
    v <- Sys.getenv(k, unset = "")
    if (nzchar(v)) return(v)
  }
  default
}

# No password is written in the code. RINSE_ADMIN_PASSWORD comes from .Renviron / the host environment
# (see .env.example). If it is not set, a random one is generated on first start and printed once (see seed_admin in db.R).
ADMIN_PW_GENERATED <- !nzchar(env("RINSE_ADMIN_PASSWORD"))
ADMIN_PW <- if (ADMIN_PW_GENERATED) sodium::bin2hex(sodium::random(10)) else env("RINSE_ADMIN_PASSWORD")

CFG <- list(
  db_path        = env("RINSE_DB", file.path("storage", "rinse.sqlite")),
  admin_email    = tolower(env("RINSE_ADMIN_EMAIL", "samikhan57705@gmail.com")),
  admin_password = ADMIN_PW,
  admin_name     = env("RINSE_ADMIN_NAME", "Md Sami Ahmad"),
  max_upload_mb  = as.numeric(env("RINSE_MAX_UPLOAD_MB", "25")),
  log_level      = env("RINSE_LOG_LEVEL", "INFO"),
  idle_minutes   = as.numeric(env("RINSE_IDLE_MINUTES", "30")),
  max_fail       = 5L,           # failed logins before lockout
  lock_minutes   = 10L,
  otp_minutes    = 5L,
  google_id      = env("GOOGLE_CLIENT_ID"),
  google_secret  = env("GOOGLE_CLIENT_SECRET"),
  google_redirect= env("GOOGLE_REDIRECT_URI", "http://localhost:3838"),
  twilio_sid     = env("TWILIO_ACCOUNT_SID"),
  twilio_token   = env("TWILIO_AUTH_TOKEN"),
  twilio_from    = env("TWILIO_FROM")
)
CFG$google_on <- nzchar(CFG$google_id) && nzchar(CFG$google_secret)
CFG$sms_on    <- nzchar(CFG$twilio_sid) && nzchar(CFG$twilio_token) && nzchar(CFG$twilio_from)
CFG$demo_sms  <- tolower(env("RINSE_DEMO_SMS")) %in% c("1", "true", "yes")   # show the Phone tab with on-screen codes (classroom demo only)

MISSING_TOKENS <- c("", "na", "n/a", "nan", "null", "none", "nil", "-", "--", "?", "missing", "unknown", "undefined", "#n/a")

# Shared helpers
`%||%` <- function(a, b) if (is.null(a) || !length(a) || (length(a) == 1 && is.na(a))) b else a
now_ts <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")
