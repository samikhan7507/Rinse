# One-time setup: installs every R package Rinse needs.
#   Rscript install_packages.R
#
# It tries fast pre-built Linux binaries first (Posit Package Manager), falls
# back to compiling from CRAN source, and finally *checks that every package
# loads*. A build that cannot provide the packages now fails loudly instead of
# producing an image that dies at runtime with "there is no package called
# 'shiny'".
pkgs <- c("shiny", "dplyr", "readxl", "writexl", "DBI", "RSQLite", "jsonlite", "httr", "sodium", "digest", "zip",
          "htmltools", "DT", "randomForest", "rpart", "nnet", "cluster", "testthat")

os_release <- tryCatch(readLines("/etc/os-release", warn = FALSE), error = function(e) character())
tag <- function(key) { v <- grep(paste0("^", key, "="), os_release, value = TRUE)[1]; if (is.na(v)) "" else sub(paste0("^", key, "="), "", v) }
distro   <- tag("ID")
codename <- tag("VERSION_CODENAME")
if (!nzchar(codename)) codename <- tag("VERSION_ID")

# Posit's binary packages need a real HTTP user agent (and the distro codename).
options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(),
  paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"])))
options(Ncpus = max(1L, parallel::detectCores(logical = FALSE)))

binary_url <- if (nzchar(codename) && (distro %in% c("ubuntu", "debian") || grepl("ubuntu|debian", tolower(tag("ID_LIKE")))))
  sprintf("https://packagemanager.posit.co/cran/__linux__/%s/latest", codename) else ""

installed <- function() rownames(utils::installed.packages())
install_from <- function(todo, repos, label) {
  todo <- setdiff(todo, installed())
  if (!length(todo)) return(invisible(FALSE))
  message("-> ", label, " (", length(todo), "): ", paste(todo, collapse = ", "))
  # Only errors are caught: a warning handler would abort the remaining installs.
  tryCatch(utils::install.packages(todo, repos = repos, quiet = TRUE),
           error = function(e) message("   install error: ", conditionMessage(e)))
  invisible(TRUE)
}

if (nzchar(binary_url)) install_from(pkgs, binary_url, paste("pre-built binaries for", codename))
# Anything the binary repository could not serve is compiled from CRAN source.
install_from(pkgs, "https://cloud.r-project.org", "source packages from CRAN")

# Final gate: the package must be installed *and* loadable (a broken shared
# library looks installed but fails when Shiny starts).
missing <- setdiff(pkgs, installed())
if (length(missing))
  stop("Failed to install: ", paste(missing, collapse = ", "),
       "\nCheck the build log above - usually a missing system library (see the Dockerfile).", call. = FALSE)
bad <- pkgs[!vapply(pkgs, function(p) isTRUE(requireNamespace(p, quietly = TRUE)), logical(1))]
if (length(bad))
  stop("Installed but cannot load: ", paste(bad, collapse = ", "), call. = FALSE)

if (getRversion() < "4.1.0") warning("Rinse needs R 4.1 or newer (it uses the native pipe).")
message("All ", length(pkgs), " R packages are ready.")
message("Start the app with:  Rscript -e \"shiny::runApp(host = '0.0.0.0', port = 3838)\"")
