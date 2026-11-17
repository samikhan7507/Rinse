# ML preparation: split first, then fit scaling on the TRAIN rows only (prevents data leakage).
DEFAULT_ML <- list(encode = "onehot", scale = "standardize", test_ratio = 0.2, seed = 42, stratify = TRUE, date_parts = TRUE)

safe_names <- function(x) gsub("^_|_$", "", gsub("[^A-Za-z0-9]+", "_", x))

prepare_ml <- function(df, target = NULL, opts = DEFAULT_ML) {
  # Validate input early to give clear errors instead of hanging
  if (is.null(df) || nrow(df) < 5) stop("Need at least 5 rows to prepare for ML. Upload a larger file or check cleaning didn't remove too much.", call. = FALSE)
  if (ncol(df) == 0) stop("No columns available for ML preparation.", call. = FALSE)
  o <- modifyList(DEFAULT_ML, opts)
  # Clamp test_ratio to sensible bounds
  if (is.null(o$test_ratio) || is.na(o$test_ratio) || o$test_ratio < 0.05) o$test_ratio <- 0.2
  if (o$test_ratio > 0.5) o$test_ratio <- 0.3
  if (is.null(o$seed) || is.na(o$seed)) o$seed <- 42
  steps <- character(0); spec <- list(dates = character(), enc = list(), date_parts = o$date_parts)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # date parts
  for (nm in names(df)[vapply(df, inherits, logical(1), "Date")]) {
    spec$dates <- c(spec$dates, nm)
    if (o$date_parts) {
      d <- df[[nm]]; parts <- paste0(nm, c("_year", "_month", "_weekday"))
      df <- dplyr::mutate(df, !!!stats::setNames(list(as.integer(format(d, "%Y")), as.integer(format(d, "%m")), as.integer(format(d, "%u"))), parts))
      steps <- c(steps, paste0("Split ", nm, " into year, month, weekday"))
      same <- parts[vapply(parts, function(p) length(unique(df[[p]][!is.na(df[[p]])])) <= 1, logical(1))]
      if (length(same)) { df <- dplyr::select(df, -dplyr::all_of(same)); steps <- c(steps, paste0("Left out ", paste(same, collapse = ", "), ": the same value in every row")) }
    }
    df[[nm]] <- NULL
  }
  task <- "none"; y <- NULL
  if (!is.null(target) && nzchar(target) && target %in% names(df)) {
    y <- df[[target]]; df[[target]] <- NULL
    task <- if (is.numeric(y) && length(unique(y)) > 10) "regression" else "classification"
  }
  # split - with guards for tiny or imbalanced data
  set.seed(o$seed); n <- nrow(df)
  if (n < 10) stop(sprintf("Only %d rows remain - too few to split into train/test. Try less aggressive cleaning.", n), call. = FALSE)
  test_idx <- integer(0)
  if (task == "classification" && o$stratify) {
    for (cl in unique(y)) {
      ix <- which(y == cl)
      if (length(ix) < 2) {
        # Too few of this class - fall back to random split for this class
        test_idx <- c(test_idx, sample(ix, max(1, round(length(ix) * o$test_ratio))))
      } else {
        want <- max(1, round(length(ix) * o$test_ratio))
        # Ensure we leave at least 1 of each class in train
        want <- min(want, length(ix) - 1)
        if (want > 0) test_idx <- c(test_idx, sample(ix, want))
      }
    }
    # If stratified left no test rows (e.g., single-class edge), fall back to random
    if (length(test_idx) == 0) test_idx <- sample(n, max(1, round(n * o$test_ratio)))
  } else {
    test_idx <- sample(n, max(1, round(n * o$test_ratio)))
  }
  test_idx <- sort(unique(test_idx)); train_idx <- setdiff(seq_len(n), test_idx)
  # Final guard: ensure both train and test have rows
  if (length(train_idx) < 3) stop("Train set would be too small. Try a smaller test share (e.g., 10%).", call. = FALSE)
  if (length(test_idx) < 1) stop("Test set would be empty. Adjust test share.", call. = FALSE)
  # encode
  cat_cols <- names(df)[!vapply(df, is.numeric, logical(1))]
  idlike <- cat_cols[vapply(cat_cols, function(nm) length(unique(df[[nm]])) > 0.9 * n, logical(1))]
  for (nm in idlike) { df[[nm]] <- NULL; steps <- c(steps, sprintf("Left out %s: a different value in almost every row", nm)) }
  cat_cols <- setdiff(cat_cols, idlike)
  for (nm in cat_cols) {
    x <- as.character(df[[nm]]); x[is.na(x)] <- "Missing"; lv <- sort(unique(x[train_idx]))
    if (o$encode == "onehot" && length(lv) <= 10 && length(lv) > 1) {
      spec$enc[[nm]] <- list(kind = "onehot", levels = lv)
      for (l in lv[-1]) df[[paste0(nm, "_", safe_names(l))]] <- as.integer(x == l)
      steps <- c(steps, sprintf("One-hot encoded %s (%d levels)", nm, length(lv)))
      df[[nm]] <- NULL
    } else if (o$encode != "none") {
      allv <- c(lv, setdiff(unique(x), lv)); spec$enc[[nm]] <- list(kind = "label", levels = allv)
      df[[nm]] <- match(x, allv) - 1L
      steps <- c(steps, sprintf("Label encoded %s", nm))
    }
  }
  # scale numeric columns that are not 0/1 dummies (train statistics only)
  params <- list()
  if (o$scale != "none") for (nm in names(df)[vapply(df, is.numeric, logical(1))]) {
    u <- unique(df[[nm]][train_idx]); if (length(u) <= 2) next
    if (o$scale == "standardize") { a <- mean(df[[nm]][train_idx]); b <- stats::sd(df[[nm]][train_idx]) }
    else { a <- min(df[[nm]][train_idx]); b <- max(df[[nm]][train_idx]) - a }
    if (!is.finite(b) || b == 0) next
    df[[nm]] <- (df[[nm]] - a) / b; params[[nm]] <- c(a = a, b = b)
  }
  if (length(params)) steps <- c(steps, sprintf("%s %d numeric columns using train rows only", if (o$scale == "standardize") "Standardized" else "Min-max scaled", length(params)))
  spec$scale <- params; spec$features <- names(df); spec$target <- target; spec$task <- task
  spec$target_levels <- if (task == "classification") sort(unique(as.character(y))) else NULL
  if (!is.null(y)) { df[[target]] <- y }
  steps <- c(steps, sprintf("Split into %d train and %d test rows%s", length(train_idx), length(test_idx), if (task == "classification" && o$stratify) " (stratified by target)" else ""))
  list(train = df[train_idx, , drop = FALSE], test = df[test_idx, , drop = FALSE], full = df, task = task, target = target,
       steps = steps, scale_params = params, options = o, spec = spec)
}

# Turn cleaned new rows into the exact feature matrix the model was trained on.
ml_transform <- function(spec, d) {
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  for (nm in spec$dates) if (nm %in% names(d)) {
    if (isTRUE(spec$date_parts)) { d[[paste0(nm, "_year")]] <- as.integer(format(d[[nm]], "%Y")); d[[paste0(nm, "_month")]] <- as.integer(format(d[[nm]], "%m")); d[[paste0(nm, "_weekday")]] <- as.integer(format(d[[nm]], "%u")) }
    d[[nm]] <- NULL
  }
  for (nm in names(spec$enc)) {
    e <- spec$enc[[nm]]; x <- if (nm %in% names(d)) as.character(d[[nm]]) else rep("Missing", nrow(d)); x[is.na(x)] <- "Missing"
    if (e$kind == "onehot") { for (l in e$levels[-1]) d[[paste0(nm, "_", safe_names(l))]] <- as.integer(x == l); d[[nm]] <- NULL }
    else d[[nm]] <- match(x, e$levels) - 1L
  }
  for (nm in names(spec$scale)) if (nm %in% names(d)) d[[nm]] <- (d[[nm]] - spec$scale[[nm]]["a"]) / spec$scale[[nm]]["b"]
  out <- as.data.frame(lapply(spec$features, function(f) { v <- if (f %in% names(d)) as.numeric(d[[f]]) else rep(0, nrow(d)); v[!is.finite(v)] <- 0; v }), col.names = spec$features, check.names = FALSE)
  out
}
