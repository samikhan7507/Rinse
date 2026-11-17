for (f in list.files("../../R", pattern = "(config|import|profile|score|clean|features|export)\\.R$", full.names = TRUE)) source(f)
samples <- list(jaipur_house_prices = "price_lakh", campus_placement = "placed", loan_default = "defaulted",
                telecom_churn = "churn", heart_screening = "heart_disease", upi_transactions = "is_fraud")
load_s <- function(n) read_any(file.path("../../data/samples", paste0(n, ".csv")))

test_that("type helpers work", {
  expect_equal(to_num(c("₹1,200", "45%", "Rs. 3,500", "abc")), c(1200, 45, 3500, NA))
  expect_true(all(is_missing(c("N/A", "?", " ", NA, "null"))))
  expect_false(is_missing("Jaipur"))
})

test_that("import rejects bad input", {
  tmp <- tempfile(fileext = ".csv"); writeLines(c("a,b", "1,2"), tmp)
  expect_error(read_any(tmp), "at least 5 rows")
  expect_error(read_any(tmp, "x.exe"), "Unsupported")
  expect_error(read_url("http://example.com/a.csv"), "https")
  expect_error(read_url("https://localhost/a.csv"), "Private")
})

for (nm in names(samples)) test_that(paste("pipeline improves score:", nm), {
  df <- load_s(nm); tg <- samples[[nm]]
  pb <- profile_data(df, tg); sb <- score_data(pb)
  cl <- clean_data(df, target = tg)
  pa <- profile_data(cl$data, tg); sa <- score_data(pa)
  expect_gt(sa$score, sb$score)
  expect_gte(sa$score, 85)
  expect_lt(sb$score, 85)
  expect_equal(sum(vapply(cl$data, function(x) sum(is.na(x)), numeric(1))), 0)
  expect_equal(sum(duplicated(cl$data)), 0)
  ml <- prepare_ml(cl$data, tg)
  expect_equal(nrow(ml$train) + nrow(ml$test), nrow(cl$data))
  expect_true(all(vapply(ml$train[setdiff(names(ml$train), tg)], is.numeric, logical(1))))
  cat(sprintf("\n%-22s %3d -> %3d  rows %d -> %d  cols %d -> %d\n", nm, sb$score, sa$score, nrow(df), nrow(cl$data), ncol(df), ncol(cl$data)))
})

test_that("no data leakage: scaling uses train stats", {
  df <- load_s("telecom_churn"); cl <- clean_data(df, target = "churn"); ml <- prepare_ml(cl$data, "churn")
  col <- names(ml$scale_params)[1]
  expect_equal(mean(ml$train[[col]]), 0, tolerance = 1e-8)
})

test_that("stratified split keeps class ratio", {
  df <- load_s("loan_default"); cl <- clean_data(df, target = "defaulted"); ml <- prepare_ml(cl$data, "defaulted")
  a <- mean(ml$train$defaulted == "yes"); b <- mean(ml$test$defaulted == "yes"); expect_lt(abs(a - b), 0.03)
})

test_that("exports are created", {
  df <- load_s("jaipur_house_prices"); tg <- "price_lakh"
  pb <- profile_data(df, tg); cl <- clean_data(df, target = tg); pa <- profile_data(cl$data, tg)
  ml <- prepare_ml(cl$data, tg)
  ctx <- list(name = "x.csv", target = tg, prof_before = pb, prof_after = pa, score_before = score_data(pb), score_after = score_data(pa), clean = cl, ml = ml)
  z <- tempfile(fileext = ".zip"); build_bundle(z, ctx)
  expect_true(file.exists(z)); fl <- utils::unzip(z, list = TRUE)$Name
  expect_true(all(c("cleaned_data.csv", "train.csv", "test.csv", "rinse_report.xlsx", "rinse_report.html", "recipe.json") %in% fl))
})
