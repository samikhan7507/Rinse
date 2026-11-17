# Rinse entry point. Run with: shiny::runApp()  (or Rscript -e "shiny::runApp(port = 3838)")
options(shiny.autoload.r = FALSE, shiny.sanitize.errors = TRUE)
suppressWarnings(try(Sys.setlocale("LC_ALL", "C.UTF-8"), silent = TRUE))
library(shiny)
# Explicitly verify required packages are available with friendly error instead of silent hang
required_pkgs <- c("dplyr", "readxl", "writexl", "DBI", "RSQLite", "jsonlite", "httr", "sodium", "digest", "zip", "DT", "randomForest", "rpart", "cluster")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  stop(sprintf("Missing required packages: %s. Run: Rscript install_packages.R", paste(missing_pkgs, collapse=", ")), call. = FALSE)
}
# Load namespaces that are used via :: to ensure they are attached for progress reporting
invisible(lapply(required_pkgs, function(p) suppressPackageStartupMessages(library(p, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))))
for (f in c("config", "logger", "db", "auth", "import", "profile", "score", "clean", "features", "ml", "export", "ui_components", "pages_ml", "pages_admin", "server"))
  source(file.path("R", paste0(f, ".R")), local = FALSE, encoding = "UTF-8")
options(shiny.maxRequestSize = CFG$max_upload_mb * 1024^2)

# The app used to be called Sift, and before that Vyrel. If an older database is lying around and the
# new path is empty, adopt it so accounts and history survive the rename.
adopt_legacy_db <- function() {
  new <- CFG$db_path
  if (file.exists(new)) return(invisible(FALSE))
  for (old in file.path("storage", c("sift.sqlite", "vyrel.sqlite"))) {
    if (!file.exists(old)) next
    if (identical(normalizePath(old, mustWork = FALSE), normalizePath(new, mustWork = FALSE))) next
    dir.create(dirname(new), showWarnings = FALSE, recursive = TRUE)
    moved <- isTRUE(suppressWarnings(file.rename(old, new))) || isTRUE(file.copy(old, new, overwrite = TRUE))
    vlog("INFO", "Adopted the database from the previous name: %s -> %s", old, new)
    return(invisible(moved))
  }
  invisible(FALSE)
}
adopt_legacy_db()
db_init()
vlog("INFO", "%s %s starting (idle sign-out %s min, Google %s, SMS %s)", APP_NAME, APP_VERSION, CFG$idle_minutes, if (CFG$google_on) "on" else "off", if (CFG$sms_on) "on" else "demo")
if (nchar(CFG$admin_password) < 10) vlog("WARN", "The administrator password is short. Change it in Settings before sharing the site publicly.")

ui <- function(request) {
  # Health check for Render, Docker and load balancers: GET /health or /?health
  probe <- paste(c(request$PATH_INFO, request$QUERY_STRING))
  if (grepl("health", paste(probe, collapse = " "), fixed = TRUE)) {
    ok <- isTRUE(db_healthy())
    return(httpResponse(if (ok) 200L else 503L, "application/json",
      jsonlite::toJSON(list(status = if (ok) "ok" else "degraded", app = APP_NAME, version = APP_VERSION, time = now_ts()), auto_unbox = TRUE)))
  }
  tagList(
    tags$head(
      tags$meta(charset = "utf-8"), tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, viewport-fit=cover"),
      tags$meta(name = "theme-color", content = "#FFFFFF"), tags$meta(name = "description", content = paste(APP_NAME, "-", APP_TAGLINE)),
      tags$title(APP_NAME), tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg"),
      # Installable app (PWA): manifest + icons, so Chrome, Edge, Android and iOS can install it.
      tags$link(rel = "manifest", href = "manifest.json"),
      tags$link(rel = "apple-touch-icon", href = "icons/apple-touch-icon.png"),
      tags$meta(name = "application-name", content = APP_NAME),
      tags$meta(name = "mobile-web-app-capable", content = "yes"),
      tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
      tags$meta(name = "apple-mobile-web-app-title", content = APP_NAME),
      tags$meta(name = "apple-mobile-web-app-status-bar-style", content = "default"),
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"), tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap"),
      tags$link(rel = "stylesheet", href = "styles.css"), tags$script(src = "app.js")),
    uiOutput("root"))
}
shinyApp(ui, server)
