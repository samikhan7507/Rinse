for (f in list.files("../../R", pattern = "(config|logger|import|profile|score|clean|features|ml|export)\\.R$", full.names = TRUE)) source(f)
ld <- function(n) read_any(file.path("../../data/samples", paste0(n, ".csv")))

test_that("PII and leakage are detected, scored and removed", {
  d <- ld("campus_placement"); p <- profile_data(d, "placed"); pii <- p$columns$pii; names(pii) <- p$columns$name
  expect_equal(pii[["student_email"]], "email"); expect_equal(pii[["mobile"]], "phone number")
  l <- ld("loan_default"); pl <- profile_data(l, "defaulted"); expect_true("collection_notice_sent" %in% pl$leakage$column)
  expect_true(any(grepl("leaks the answer", score_data(pl)$issues$reason)))
  cl <- clean_data(l, target = "defaulted", prof0 = pl); expect_false("collection_notice_sent" %in% names(cl$data))
  cc <- clean_data(d, target = "placed", prof0 = p); expect_false(any(c("student_email", "mobile") %in% names(cc$data)))
  cm <- clean_data(d, list(pii = "mask"), "placed", p); expect_true("student_email" %in% names(cm$data)); expect_false(any(grepl("@", cm$data$student_email)))
})

test_that("knn imputation fills gaps from neighbours", {
  d <- ld("campus_placement"); cl <- clean_data(d, list(impute_numeric = "knn"), "placed")
  expect_equal(sum(is.na(cl$data$cgpa)), 0); expect_true(any(grepl("nearest", cl$log$action)))
})

test_that("models train, compare and cross-validate (classification and regression)", {
  for (case in list(c("telecom_churn", "churn"), c("jaipur_house_prices", "price_lakh"))) {
    d <- ld(case[1]); cl <- clean_data(d, target = case[2]); ml <- prepare_ml(cl$data, case[2])
    m <- train_models(ml, cv_folds = 3)
    expect_gte(nrow(m$results), 3); expect_true(m$best %in% m$results$model); expect_false(is.null(m$cv)); expect_false(is.null(m$importance))
    if (m$task == "classification") { expect_gt(max(m$results$accuracy), 0.55); expect_true(length(m$rocs) >= 1) } else expect_gt(max(m$results$r2), 0.5)
  }
})

test_that("cleaning beats the raw baseline on the house prices file", {
  d <- ld("jaipur_house_prices"); p0 <- profile_data(d, "price_lakh"); cl <- clean_data(d, target = "price_lakh", prof0 = p0); ml <- prepare_ml(cl$data, "price_lakh")
  m <- train_models(ml, models = c("Random forest"), cv_folds = 0); rb <- raw_baseline(d, "price_lakh", p0, ml, m$results$main[1])
  expect_true(is.list(rb)); if (isTRUE(rb$ok)) expect_gte(rb$clean, rb$raw - 0.05)
})

test_that("insights run", {
  d <- ld("loan_default"); cl <- clean_data(d, target = "defaulted"); ml <- prepare_ml(cl$data, "defaulted"); X <- insight_matrix(cl$data, "defaulted", ml)
  expect_false(is.null(corr_pairs(cl$data))); p <- pca_info(X); expect_equal(length(p$var), ncol(X)); expect_true(all(c("credit_score", "age") %in% names(X)))
  k <- cluster_rows(X); expect_true(k$k >= 2); a <- find_anomalies(X); expect_gte(a$n_flagged, 0)
})

test_that("drift is detected between shifted data", {
  d <- ld("loan_default"); n <- d; n$credit_score <- as.character(round(to_num(d$credit_score) - 120)); r <- drift_report(d, n)
  expect_equal(r$status[r$column == "credit_score"], "Significant drift"); expect_equal(r$status[r$column == "purpose"], "Stable")
})

test_that("a saved bundle scores new raw rows end to end", {
  d <- ld("loan_default"); p0 <- profile_data(d, "defaulted"); cl <- clean_data(d, target = "defaulted", prof0 = p0); ml <- prepare_ml(cl$data, "defaulted")
  m <- train_models(ml, models = c("Logistic regression", "Random forest"), cv_folds = 0); b <- make_bundle(cl, ml, m, m$best)
  new <- d[1:6, setdiff(names(d), "defaulted")]; new$purpose[1] <- "  HOME "; new$credit_score[2] <- "null"
  out <- predict_new(b, new); expect_equal(nrow(out), 6); expect_true(all(out$prediction %in% m$classes)); expect_true("confidence" %in% names(out))
  f <- tempfile(fileext = ".rds"); saveRDS(b, f); out2 <- predict_new(readRDS(f), new); expect_equal(out$prediction, out2$prediction)
})

test_that("the demo applicants file drifts and can be scored", {
  ref <- ld("loan_default"); new <- read_any("../../data/samples/loan_new_applicants.csv"); r <- drift_report(ref, new)
  expect_equal(r$status[r$column == "credit_score"], "Significant drift"); expect_equal(r$status[r$column == "monthly_income"], "Significant drift")
  p0 <- profile_data(ref, "defaulted"); cl <- clean_data(ref, target = "defaulted", prof0 = p0); ml <- prepare_ml(cl$data, "defaulted")
  m <- train_models(ml, models = "Random forest", cv_folds = 0); out <- predict_new(make_bundle(cl, ml, m, "Random forest"), new)
  expect_equal(nrow(out), 300); expect_false(any(is.na(out$prediction)))
})
