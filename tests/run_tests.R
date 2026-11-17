# Run everything: Rscript tests/run_tests.R   (from the project root)
Sys.setenv(RINSE_DB = tempfile(fileext = ".sqlite"))
testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)
source("tests/e2e.R")
