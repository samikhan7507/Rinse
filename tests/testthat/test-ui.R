# Load every module the app loads, in the same order as app.R, then check the
# plain-data dictionaries the UI is built from.
#
# Why: ICON, SAMPLES, ADMIN_TABS and friends are evaluated when the files are
# sourced, so a typo such as a trailing comma inside ICON only shows up when the
# app starts - and the error names an argument number, not the file. Loading the
# modules here turns that into an obvious CI failure.

Sys.setenv(RINSE_DB = tempfile(fileext = ".sqlite"))
suppressPackageStartupMessages(library(shiny))

modules <- c("config", "logger", "db", "auth", "import", "profile", "score", "clean",
             "features", "ml", "export", "ui_components", "pages_ml", "pages_admin", "server")

test_that("every module loads", {
  for (f in modules) {
    p <- file.path("../../R", paste0(f, ".R"))
    expect_true(file.exists(p), info = p)
    expect_error(suppressPackageStartupMessages(source(p, local = FALSE, encoding = "UTF-8")), NA,
                 info = paste("sourcing", p))
  }
})

test_that("ICON has one complete svg per navigation icon", {
  expect_true(is.list(ICON))
  expect_identical(length(ICON), 10L)
  expect_identical(names(ICON), c("analyze", "predict", "drift", "history", "admin",
                                  "settings", "help", "more", "install", "upload"))
  bad <- names(ICON)[!vapply(ICON, function(s) {
    is.character(s) && length(s) == 1L && !is.na(s) && nzchar(s) &&
      grepl("^<svg ", s) && grepl("</svg>$", s)
  }, logical(1))]
  expect_identical(bad, character(0))
})

test_that("sample catalogue is complete", {
  expect_identical(length(SAMPLES), 6L)
  expect_identical(sort(names(SAMPLES)),
                   sort(c("jaipur_house_prices", "campus_placement", "loan_default",
                          "telecom_churn", "heart_screening", "upi_transactions")))
  for (nm in names(SAMPLES)) {
    s <- SAMPLES[[nm]]
    expect_true(all(c("title", "target", "desc") %in% names(s)), info = nm)
    expect_true(all(nzchar(c(s$title, s$target, s$desc))), info = nm)
    expect_true(file.exists(file.path("../../data/samples", paste0(nm, ".csv"))), info = nm)
  }
})

test_that("tabs, steps, palette and models are intact", {
  expect_gt(length(ADMIN_TABS), 0L)
  expect_true(all(nzchar(ADMIN_TABS)))
  expect_identical(length(STEPS), 7L)
  expect_identical(length(PALETTE), 6L)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", PALETTE)))
  expect_true(length(MODELS_CLS) >= 4L && length(MODELS_REG) >= 4L)
  expect_true(all(nzchar(c(APP_NAME, APP_VERSION))))
  expect_true(grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", APP_VERSION))
})
