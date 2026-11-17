# Machine learning: model comparison, cross-validation, importance, raw-vs-clean benchmark,
# clustering, PCA, anomaly detection, drift and prediction on new rows.
# Uses only packages that ship with, or are standard for, R: stats, rpart, randomForest, class, nnet, cluster.

MODELS_CLS <- c("Logistic regression", "Decision tree", "Random forest", "k-nearest neighbours")
MODELS_REG <- c("Linear regression", "Decision tree", "Random forest", "k-nearest neighbours")

.rn <- function(X) { X <- as.data.frame(X); names(X) <- paste0("x", seq_len(ncol(X))); X }

fit_one <- function(name, X, y, task, seed = 42) {
  set.seed(seed); X <- .rn(X); d <- cbind(X, y = y)
  switch(name,
    "Logistic regression" = if (nlevels(y) == 2) list(kind = "glm", m = suppressWarnings(stats::glm(y ~ ., data = d, family = stats::binomial(), control = stats::glm.control(maxit = 50))), lv = levels(y))
                            else list(kind = "multinom", m = nnet::multinom(y ~ ., data = d, trace = FALSE, MaxNWts = 30000, maxit = 150), lv = levels(y)),
    "Linear regression"   = list(kind = "lm", m = stats::lm(y ~ ., data = d)),
    "Decision tree"       = list(kind = "rpart", m = rpart::rpart(y ~ ., data = d, method = if (task == "classification") "class" else "anova",
                                  control = rpart::rpart.control(cp = 0.003, maxdepth = 8, minsplit = 10)), lv = if (task == "classification") levels(y)),
    "Random forest"       = list(kind = "rf", m = randomForest::randomForest(x = X, y = y, ntree = 200, importance = TRUE), lv = if (task == "classification") levels(y)),
    "k-nearest neighbours" = { M <- as.matrix(X); mu <- colMeans(M); sg <- apply(M, 2, stats::sd); sg[!is.finite(sg) | sg == 0] <- 1
                               list(kind = "knn", X = scale(M, mu, sg), y = y, mu = mu, sg = sg, k = max(3, min(15, round(sqrt(nrow(M)) / 2) * 2 + 1)), lv = if (task == "classification") levels(y)) },
    stop("Unknown model"))
}

knn_predict <- function(f, Xn, classification) {
  Xn <- scale(as.matrix(Xn), f$mu, f$sg); n <- nrow(Xn); out <- if (classification) matrix(0, n, length(f$lv), dimnames = list(NULL, f$lv)) else numeric(n)
  for (i in seq_len(n)) {
    d <- rowSums((f$X - matrix(Xn[i, ], nrow(f$X), ncol(f$X), byrow = TRUE))^2); nb <- order(d)[seq_len(f$k)]
    if (classification) out[i, ] <- as.numeric(table(factor(f$y[nb], levels = f$lv))) / f$k else out[i] <- mean(f$y[nb])
  }
  out
}

# Returns list(class, prob) for classification, list(value) for regression.
pred_one <- function(f, X, task) {
  X <- .rn(X); cls <- task == "classification"
  if (f$kind == "knn") { r <- knn_predict(f, X, cls); return(if (cls) list(class = factor(f$lv[max.col(r, ties.method = "first")], levels = f$lv), prob = r) else list(value = r)) }
  if (f$kind == "glm") { p <- suppressWarnings(stats::predict(f$m, X, type = "response")); pr <- cbind(1 - p, p); colnames(pr) <- f$lv; return(list(class = factor(f$lv[(p > .5) + 1], levels = f$lv), prob = pr)) }
  if (f$kind == "multinom") { pr <- stats::predict(f$m, X, type = "probs"); if (is.null(dim(pr))) pr <- cbind(1 - pr, pr); colnames(pr) <- f$lv; return(list(class = factor(f$lv[max.col(pr, ties.method = "first")], levels = f$lv), prob = pr)) }
  if (f$kind == "lm") return(list(value = as.numeric(suppressWarnings(stats::predict(f$m, X)))))
  if (f$kind == "rpart") { if (cls) { pr <- stats::predict(f$m, X, type = "prob"); return(list(class = factor(f$lv[max.col(pr, ties.method = "first")], levels = f$lv), prob = pr)) } else return(list(value = as.numeric(stats::predict(f$m, X)))) }
  if (f$kind == "rf") { if (cls) { pr <- stats::predict(f$m, X, type = "prob"); return(list(class = factor(f$lv[max.col(pr, ties.method = "first")], levels = f$lv), prob = pr)) } else return(list(value = as.numeric(stats::predict(f$m, X)))) }
}

auc_binary <- function(score, is_pos) {
  n1 <- sum(is_pos); n0 <- sum(!is_pos); if (n1 == 0 || n0 == 0) return(NA_real_)
  r <- rank(score); (sum(r[is_pos]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

roc_points <- function(score, is_pos, n = 60) {
  th <- sort(unique(c(0, stats::quantile(score, seq(0, 1, length.out = n)), 1)), decreasing = TRUE)
  data.frame(fpr = vapply(th, function(t) mean(score[!is_pos] >= t), 0), tpr = vapply(th, function(t) mean(score[is_pos] >= t), 0))
}

metrics_cls <- function(actual, pred, prob, pos) {
  lv <- levels(actual); cm <- table(Actual = actual, Predicted = factor(pred, levels = lv))
  rec <- diag(cm) / pmax(1, rowSums(cm)); prec <- diag(cm) / pmax(1, colSums(cm)); f1 <- ifelse(prec + rec == 0, 0, 2 * prec * rec / (prec + rec))
  list(accuracy = sum(diag(cm)) / sum(cm), balanced = mean(rec), f1 = mean(f1), auc = if (length(lv) == 2) auc_binary(prob[, pos], actual == pos) else NA_real_, cm = cm)
}
metrics_reg <- function(actual, pred) {
  e <- actual - pred; list(rmse = sqrt(mean(e^2)), mae = mean(abs(e)), r2 = 1 - sum(e^2) / sum((actual - mean(actual))^2))
}

# ---- Train, compare, cross-validate ----
train_models <- function(ml, models = NULL, seed = 42, max_train = 6000, cv_folds = 5, progress = NULL) {
  task <- ml$task; if (task == "none") stop("Choose a target column in step 1 to train models.", call. = FALSE)
  sp <- ml$spec; feats <- sp$features; tg <- ml$target
  tr <- ml$train; te <- ml$test; if (nrow(tr) > max_train) tr <- tr[sample(nrow(tr), max_train), , drop = FALSE]
  if (nrow(te) > 3000) te <- te[sample(nrow(te), 3000), , drop = FALSE]
  cls <- task == "classification"
  Xtr <- tr[, feats, drop = FALSE]; Xte <- te[, feats, drop = FALSE]
  if (cls) { lv <- sp$target_levels; ytr <- factor(tr[[tg]], levels = lv); yte <- factor(te[[tg]], levels = lv); pos <- names(which.min(table(ytr)))[1]; if (length(lv) == 2) pos <- names(sort(table(ytr)))[1] }
  else { ytr <- as.numeric(tr[[tg]]); yte <- as.numeric(te[[tg]]); lv <- NULL; pos <- NULL }
  if (cls && nlevels(ytr) < 2) stop("The target needs at least two classes in the training data. Check if cleaning removed a rare class - try keeping more data (lower outlier removal, higher missing threshold).", call. = FALSE)
  if (!cls && length(unique(ytr)) < 2) stop("The numeric target has only one unique value in training data. It cannot be modeled.", call. = FALSE)
  models <- models %||% (if (cls) MODELS_CLS else MODELS_REG); notes <- character(0)
  fits <- list(); rows <- list(); cms <- list(); rocs <- list(); preds <- list()
  for (i in seq_along(models)) {
    nm <- models[i]; if (!is.null(progress)) progress(i, length(models), nm)
    t0 <- proc.time()[["elapsed"]]
    res <- tryCatch({
      f <- fit_one(nm, Xtr, ytr, task, seed); p <- pred_one(f, Xte, task); list(f = f, p = p)
    }, error = function(e) { notes <<- c(notes, paste0(nm, " was skipped: ", conditionMessage(e))); NULL })
    if (is.null(res)) next
    sec <- proc.time()[["elapsed"]] - t0
    if (cls) { m <- metrics_cls(yte, res$p$class, res$p$prob, pos); cms[[nm]] <- m$cm
               if (length(lv) == 2) rocs[[nm]] <- roc_points(res$p$prob[, pos], yte == pos)
               rows[[nm]] <- data.frame(model = nm, accuracy = m$accuracy, balanced = m$balanced, f1 = m$f1, auc = m$auc, main = m$f1, main_metric = "F1 (macro)", seconds = sec, stringsAsFactors = FALSE)
               preds[[nm]] <- data.frame(actual = as.character(yte), predicted = as.character(res$p$class), confidence = round(apply(res$p$prob, 1, max), 4), stringsAsFactors = FALSE) }
    else { m <- metrics_reg(yte, res$p$value)
           rows[[nm]] <- data.frame(model = nm, rmse = m$rmse, mae = m$mae, r2 = m$r2, main = m$r2, main_metric = "R squared", seconds = sec, stringsAsFactors = FALSE)
           preds[[nm]] <- data.frame(actual = yte, predicted = round(res$p$value, 4), error = round(res$p$value - yte, 4), stringsAsFactors = FALSE) }
    fits[[nm]] <- res$f
  }
  if (!length(rows)) stop("No model could be trained on this data. ", paste(notes, collapse = " "), call. = FALSE)
  results <- do.call(rbind, rows); rownames(results) <- NULL; results <- dplyr::arrange(results, dplyr::desc(main))
  best <- results$model[1]
  base <- if (cls) { mj <- names(which.max(table(ytr))); data.frame(label = "Always guess the most common class", value = mean(yte == mj), metric = "Accuracy") } else data.frame(label = "Always predict the average", value = 0, metric = "R squared")
  # cross-validation of the best model on the training rows
  cv <- NULL
  if (cv_folds >= 2 && nrow(Xtr) >= 50) cv <- tryCatch({
    set.seed(seed); fold <- sample(rep(seq_len(cv_folds), length.out = nrow(Xtr)))
    sc <- vapply(seq_len(cv_folds), function(k) { a <- fold != k; f <- fit_one(best, Xtr[a, , drop = FALSE], ytr[a], task, seed); p <- pred_one(f, Xtr[!a, , drop = FALSE], task)
      if (cls) metrics_cls(ytr[!a], p$class, p$prob, pos)$f1 else metrics_reg(ytr[!a], p$value)$r2 }, numeric(1))
    data.frame(model = best, metric = results$main_metric[1], mean = mean(sc), sd = stats::sd(sc), folds = cv_folds)
  }, error = function(e) NULL)
  list(task = task, results = results, best = best, fits = fits, cms = cms, rocs = rocs, preds = preds, importance = importance_of(fits, feats), cv = cv, baseline = base, notes = notes,
       classes = lv, positive = pos, features = feats, target = tg, trained_at = now_ts())
}

importance_of <- function(fits, feats) {
  f <- fits[["Random forest"]]; imp <- NULL
  if (!is.null(f)) { m <- randomForest::importance(f$m); col <- if ("MeanDecreaseAccuracy" %in% colnames(m)) "MeanDecreaseAccuracy" else "%IncMSE"; v <- m[, col] }
  else if (!is.null(fits[["Decision tree"]])) { v <- fits[["Decision tree"]]$m$variable.importance; v <- v[paste0("x", seq_along(feats))]; v[is.na(v)] <- 0 }
  else return(NULL)
  idx <- as.integer(sub("^x", "", names(v))); d <- data.frame(feature = feats[idx], importance = as.numeric(v), stringsAsFactors = FALSE)
  d$importance <- pmax(0, d$importance); d$share <- round(100 * d$importance / max(1e-12, sum(d$importance)), 1)
  dplyr::arrange(d, dplyr::desc(importance))
}

# ---- Does cleaning actually help? Train on the naive version of the raw file and compare. ----
raw_baseline <- function(raw, target, prof0, ml, clean_main, seed = 42) {
  tryCatch({
    tp <- stats::setNames(prof0$columns$type, prof0$columns$name); d <- raw
    for (nm in names(d)) d[[nm]] <- if (identical(tp[[nm]], "numeric")) to_num(d[[nm]]) else { z <- as.character(d[[nm]]); z[is.na(z)] <- "NA"; z }
    d <- d[stats::complete.cases(d), , drop = FALSE]; if (nrow(d) < 40) return(list(ok = FALSE, msg = sprintf("Only %d rows are left if you simply delete rows with gaps, so a baseline model is not possible.", nrow(d))))
    task <- ml$task; y <- d[[target]]; X <- d[, setdiff(names(d), target), drop = FALSE]
    for (nm in names(X)) if (!is.numeric(X[[nm]])) X[[nm]] <- as.numeric(factor(X[[nm]]))
    set.seed(seed); idx <- sample(nrow(X), round(nrow(X) * ml$options$test_ratio)); tr <- setdiff(seq_len(nrow(X)), idx)
    if (task == "classification") { y <- factor(y); if (nlevels(y) < 2 || min(table(y[tr])) < 2) return(list(ok = FALSE, msg = "Too few rows per class remain in the raw data.")) } else y <- to_num(y)
    f <- fit_one("Random forest", X[tr, , drop = FALSE], y[tr], task, seed); p <- pred_one(f, X[idx, , drop = FALSE], task)
    main <- if (task == "classification") { pos <- names(sort(table(y[tr])))[1]; metrics_cls(factor(y[idx], levels = levels(y)), p$class, p$prob, pos)$f1 } else metrics_reg(y[idx], p$value)$r2
    list(ok = TRUE, rows_kept = nrow(d), rows_total = nrow(raw), raw = main, clean = clean_main, metric = if (task == "classification") "F1 (macro)" else "R squared")
  }, error = function(e) list(ok = FALSE, msg = paste("The raw file could not be modelled without cleaning:", conditionMessage(e))))
}

# ---- Insights ----
corr_pairs <- function(df, top = 12) {
  num <- df[, vapply(df, is.numeric, logical(1)), drop = FALSE]; num <- num[, vapply(num, function(z) stats::sd(z, na.rm = TRUE) > 0, logical(1)), drop = FALSE]
  if (ncol(num) < 2) return(NULL); if (ncol(num) > 25) num <- num[, seq_len(25), drop = FALSE]
  cm <- suppressWarnings(stats::cor(num, use = "pairwise.complete.obs")); idx <- which(upper.tri(cm), arr.ind = TRUE)
  p <- data.frame(a = colnames(cm)[idx[, 1]], b = colnames(cm)[idx[, 2]], r = round(cm[idx], 3), stringsAsFactors = FALSE)
  list(matrix = cm, pairs = utils::head(p[order(-abs(p$r)), ], top))
}

feature_matrix <- function(ml, max_rows = 3000) {
  X <- ml$full[, ml$spec$features, drop = FALSE]; X <- X[, vapply(X, function(z) stats::sd(z) > 0, logical(1)), drop = FALSE]
  if (ncol(X) < 2) return(NULL); if (nrow(X) > max_rows) X <- X[sample(nrow(X), max_rows), , drop = FALSE]; X
}

# Insights use the readable numeric columns of the cleaned data (not one-hot dummies), so cluster profiles are in real units.
insight_matrix <- function(clean_df, target, ml, max_rows = 3000) {
  num <- clean_df[, vapply(clean_df, is.numeric, logical(1)), drop = FALSE]; num <- num[, setdiff(names(num), target), drop = FALSE]
  num <- num[, vapply(num, function(z) length(unique(z)) > 2 && stats::sd(z) > 0, logical(1)), drop = FALSE]
  if (ncol(num) < 2) return(feature_matrix(ml, max_rows))
  if (nrow(num) > max_rows) num <- num[sample(nrow(num), max_rows), , drop = FALSE]
  num
}

pca_info <- function(X) {
  p <- stats::prcomp(X, scale. = TRUE); v <- p$sdev^2 / sum(p$sdev^2)
  list(var = v, cum = cumsum(v), scores = p$x[, 1:2, drop = FALSE], n90 = which(cumsum(v) >= .9)[1], loadings = p$rotation[, 1:2, drop = FALSE])
}

cluster_rows <- function(X, kmax = 6, seed = 42) {
  set.seed(seed); Z <- scale(X); Z[!is.finite(Z)] <- 0; sub <- if (nrow(Z) > 1200) Z[sample(nrow(Z), 1200), , drop = FALSE] else Z
  d <- stats::dist(sub); best <- NULL; sc <- numeric()
  for (k in 2:kmax) { km <- stats::kmeans(sub, k, nstart = 5); s <- mean(cluster::silhouette(km$cluster, d)[, 3]); sc[as.character(k)] <- s; if (is.null(best) || s > best$s) best <- list(k = k, s = s, km = km) }
  km <- stats::kmeans(Z, best$k, nstart = 10); prof <- as.data.frame(t(sapply(split(as.data.frame(X), km$cluster), colMeans)))
  spread <- apply(prof, 2, stats::sd); top <- names(sort(spread, decreasing = TRUE))[seq_len(min(5, ncol(prof)))]
  list(k = best$k, silhouette = round(best$s, 3), scores = round(sc, 3), size = as.integer(table(km$cluster)), centers = round(prof[, top, drop = FALSE], 2), labels = km$cluster)
}

find_anomalies <- function(X, top = 15, p = 0.999) {
  X <- X[, apply(X, 2, function(z) length(unique(z)) > 2), drop = FALSE]; if (ncol(X) < 2) return(NULL); if (ncol(X) > 30) X <- X[, seq_len(30), drop = FALSE]
  S <- stats::cov(X) + diag(1e-6, ncol(X)); d2 <- stats::mahalanobis(X, colMeans(X), S); cut <- stats::qchisq(p, ncol(X))
  o <- order(-d2); list(n_flagged = sum(d2 > cut), pct = round(100 * mean(d2 > cut), 2), cutoff = round(cut, 1), top = data.frame(row = rownames(X)[o][seq_len(min(top, length(o)))], distance = round(d2[o][seq_len(min(top, length(o)))], 1), stringsAsFactors = FALSE), all = d2)
}

# ---- Drift: has new data moved away from the data a model was built on? (Population Stability Index) ----
psi <- function(a, b, bins = 10) {
  qs <- unique(stats::quantile(a, seq(0, 1, length.out = bins + 1), na.rm = TRUE)); if (length(qs) < 3) return(0)
  qs[1] <- -Inf; qs[length(qs)] <- Inf
  pa <- as.numeric(table(cut(a, qs))) / sum(!is.na(a)); pb <- as.numeric(table(cut(b, qs))) / max(1, sum(!is.na(b)))
  pa <- pmax(pa, 1e-4); pb <- pmax(pb, 1e-4); sum((pb - pa) * log(pb / pa))
}
psi_cat <- function(a, b) {
  lv <- union(unique(a), unique(b)); pa <- as.numeric(table(factor(a, levels = lv))) / length(a); pb <- as.numeric(table(factor(b, levels = lv))) / length(b)
  pa <- pmax(pa, 1e-4); pb <- pmax(pb, 1e-4); sum((pb - pa) * log(pb / pa))
}
drift_report <- function(ref, new) {
  cols <- intersect(names(ref), names(new)); if (!length(cols)) stop("The two files share no column names.", call. = FALSE)
  rows <- lapply(cols, function(nm) {
    a <- ref[[nm]]; b <- new[[nm]]; ta <- infer_type(a, nm)$type
    if (ta == "numeric") { va <- to_num(a); vb <- to_num(b); s <- psi(va, vb); data.frame(column = nm, type = "numeric", psi = round(s, 3), ref_mean = round(mean(va, na.rm = TRUE), 2), new_mean = round(mean(vb, na.rm = TRUE), 2), new_missing_pct = round(100 * mean(is_missing(b)), 1)) }
    else if (ta %in% c("categorical", "boolean")) { s <- psi_cat(trimws(a[!is_missing(a)]), trimws(b[!is_missing(b)])); data.frame(column = nm, type = "category", psi = round(s, 3), ref_mean = NA, new_mean = NA, new_missing_pct = round(100 * mean(is_missing(b)), 1)) }
    else NULL })
  d <- do.call(rbind, rows); if (is.null(d)) stop("No comparable numeric or category columns were found.", call. = FALSE)
  d$status <- ifelse(d$psi >= 0.25, "Significant drift", ifelse(d$psi >= 0.1, "Moderate drift", "Stable"))
  dplyr::arrange(d, dplyr::desc(psi))
}

# ---- Prediction bundle: everything needed to score new raw rows ----
make_bundle <- function(clean, ml, mod, model_name) {
  list(app = APP_NAME, version = APP_VERSION, created = now_ts(), model_name = model_name, fit_obj = mod$fits[[model_name]], task = mod$task, classes = mod$classes, positive = mod$positive,
       features = mod$features, target = mod$target, clean_fit = clean$fit, spec = ml$spec, raw_columns = setdiff(unique(sub("_was_missing$", "", clean$fit$kept)), mod$target),
       metrics = mod$results[mod$results$model == model_name, , drop = FALSE])
}
predict_new <- function(bundle, raw_new) {
  cn <- clean_new(bundle$clean_fit, raw_new); X <- ml_transform(bundle$spec, cn); p <- pred_one(bundle$fit_obj, X, bundle$task)
  out <- as.data.frame(raw_new, stringsAsFactors = FALSE)
  if (bundle$task == "classification") { out$prediction <- as.character(p$class); out$confidence <- round(apply(p$prob, 1, max), 3); if (length(bundle$classes) == 2) out[[paste0("probability_", bundle$positive)]] <- round(p$prob[, bundle$positive], 3) }
  else out$prediction <- round(p$value, 3)
  out
}
