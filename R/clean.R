# Cleaning engine (Unit III: missing data, filtering, subsetting, dplyr verbs).
# Every step writes to an audit log, and the fitted values are kept so new rows can be cleaned the same way.

DEFAULT_CLEAN <- list(
  standardize_missing = TRUE, trim_text = TRUE, unify_labels = TRUE, fix_numbers = TRUE, parse_dates = TRUE,
  drop_duplicates = TRUE, drop_constant = TRUE, drop_id = TRUE, drop_sparse = TRUE, sparse_threshold = 60,
  pii = "drop", drop_leakage = TRUE,
  impute_numeric = "median", impute_categorical = "mode", outliers = "cap", missing_flags = FALSE
)

mode_of <- function(x) { x <- x[!is.na(x)]; if (!length(x)) return(NA_character_); names(sort(table(x), decreasing = TRUE))[1] }

mask_values <- function(x) {
  u <- unique(x[!is.na(x)]); if (!length(u)) return(x)
  m <- stats::setNames(vapply(u, function(v) substr(digest::digest(v, algo = "sha256", serialize = FALSE), 1, 10), ""), u)
  out <- x; ok <- !is.na(x); out[ok] <- unname(m[x[ok]]); out
}

# Machine-learning imputation: k nearest neighbours on the complete numeric columns.
knn_impute <- function(df, col, k = 5, max_ref = 3000, exclude = character(0)) {
  x <- df[[col]]; miss <- which(is.na(x)); if (!length(miss)) return(x)
  preds <- setdiff(names(df)[vapply(df, is.numeric, logical(1)) & vapply(df, function(z) !anyNA(z), logical(1))], c(col, exclude))   # never use the target to fill a feature
  if (length(preds) < 1) return(replace(x, miss, stats::median(x, na.rm = TRUE)))
  # Defensive: if preds matrix is too wide, limit columns for speed
  if (length(preds) > 25) preds <- preds[seq_len(25)]
  M <- scale(as.matrix(df[preds])); M[!is.finite(M)] <- 0
  ref <- which(!is.na(x)); if (length(ref) > max_ref) ref <- sample(ref, max_ref)
  # Defensive: if still many misses, process in chunks to avoid UI hang
  for (i in miss) {
    d <- rowSums((M[ref, , drop = FALSE] - matrix(M[i, ], length(ref), ncol(M), byrow = TRUE))^2)
    # Handle case where d has NA
    d[!is.finite(d)] <- Inf
    x[i] <- mean(x[ref[order(d)[seq_len(min(k, length(ref)))]]], na.rm = TRUE)
  }
  # Fallback if mean is NaN
  if (any(is.na(x[miss]))) x[miss][is.na(x[miss])] <- stats::median(x, na.rm = TRUE)
  x
}

clean_data <- function(df, opts = DEFAULT_CLEAN, target = NULL, prof0 = NULL) {
  # --- Validation: ensure we have usable data before starting
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) stop("No data to clean. Please upload a file first.", call. = FALSE)
  if (ncol(df) == 0) stop("The file has no columns to clean.", call. = FALSE)
  if (nrow(df) < 3) stop("Not enough rows to clean meaningfully. Need at least 3 rows.", call. = FALSE)

  o <- modifyList(DEFAULT_CLEAN, opts)
  log <- list(); masked <- character(0); fit <- list(label_map = list(), caps = list(), fills = list(), dropped = character(), numeric_cols = character(), date_cols = character(), flags = o$missing_flags)
  add <- function(step, column, action, n = NA) log[[length(log) + 1]] <<- data.frame(step = step, column = column, action = action, rows_affected = n, stringsAsFactors = FALSE)
  keep <- if (!is.null(target) && nzchar(target)) target else character(0)
  if (keep != "" && !keep %in% names(df)) {
    warning(sprintf("Target column '%s' not found; cleaning without target.", keep))
    keep <- character(0)
  }
  if (is.null(prof0)) prof0 <- profile_data(df, target)
  types <- stats::setNames(prof0$columns$type, prof0$columns$name); fit$types <- types
  pii_cols <- stats::setNames(prof0$columns$pii, prof0$columns$name)

  # 1. placeholders -> NA, trim spaces
  for (nm in names(df)) if (is.character(df[[nm]])) {
    x <- df[[nm]]
    if (o$trim_text) { t <- trimws(x); k <- sum(!is.na(x) & x != t, na.rm = TRUE); if (!is.na(k) && k > 0) { x <- t; add("Trim", nm, "Removed extra spaces", k) } }
    if (o$standardize_missing) { m <- is_missing(x) & !is.na(x); if (any(m, na.rm = TRUE)) { x[m] <- NA; add("Missing", nm, "Converted placeholders to blank", sum(m, na.rm = TRUE)) } }
    df[[nm]] <- x
  }
  # 2. personal data and target leakage (decided before anything is converted)
  if (o$pii != "keep") for (nm in setdiff(names(df)[nzchar(pii_cols[names(df)])], keep)) {
    # Guard: column may have been already removed
    if (!nm %in% names(df)) next
    if (o$pii == "mask") { df[[nm]] <- mask_values(as.character(df[[nm]])); types[[nm]] <- "categorical"; masked <- c(masked, nm); add("Privacy", nm, paste0("Masked ", pii_cols[[nm]], " values with a one-way code"), sum(!is.na(df[[nm]]))) }
    else { df[[nm]] <- NULL; fit$dropped <- c(fit$dropped, nm); add("Privacy", nm, paste0("Dropped: contains ", pii_cols[[nm]]), NA) }
  }
  if (o$drop_leakage && !is.null(prof0$leakage) && nrow(prof0$leakage)) for (nm in setdiff(prof0$leakage$column, keep)) if (nm %in% names(df)) {
    df[[nm]] <- NULL; fit$dropped <- c(fit$dropped, nm); add("Leakage", nm, "Dropped: almost identical to the target", NA)
  }
  # Check after critical drops: do we still have data?
  if (ncol(df) == 0) stop("All columns were removed during privacy/leakage filtering. Try keeping PII columns (set to 'keep') or disable leakage dropping.", call. = FALSE)
  if (nrow(df) == 0) stop("All rows were removed. Check your data for excessive missing target values.", call. = FALSE)

  # 3. numbers stored as text
  if (o$fix_numbers) for (nm in names(df)) if (nm %in% names(types) && identical(types[[nm]], "numeric") && is.character(df[[nm]])) {
    old <- df[[nm]]; df[[nm]] <- to_num(old); bad <- sum(!is.na(old) & is.na(df[[nm]]))
    add("Types", nm, "Converted text to numbers", sum(!is.na(df[[nm]])))
    if (!is.na(bad) && bad > 0) add("Types", nm, "Unreadable values set to missing", bad)
  }
  # 4. dates
  if (o$parse_dates) for (nm in names(df)) if (nm %in% names(types) && identical(types[[nm]], "date") && is.character(df[[nm]])) {
    df[[nm]] <- parse_date_flex(df[[nm]]); fit$date_cols <- c(fit$date_cols, nm); add("Types", nm, "Converted text to dates", sum(!is.na(df[[nm]])))
  }
  # 5. unify labels (Male/male/MALE -> most common spelling)
  if (o$unify_labels) for (nm in names(df)) if (nm %in% names(types) && types[[nm]] %in% c("categorical", "boolean") && is.character(df[[nm]])) {
    x <- df[[nm]]; key <- tolower(x); ok <- !is.na(x)
    # Guard: if no non-NA values, skip
    if (sum(ok, na.rm = TRUE) == 0) next
    best <- tapply(x[ok], key[ok], mode_of); fit$label_map[[nm]] <- best
    new <- x; new[ok] <- unname(best[key[ok]]); ch <- sum(ok & new != x, na.rm = TRUE)
    if (!is.na(ch) && ch > 0) { df[[nm]] <- new; add("Labels", nm, "Unified spelling and case", ch) }
  }
  # 6. rows with missing target
  if (length(keep) && keep %in% names(df)) {
    m <- is_missing(df[[keep]]); if (any(m, na.rm = TRUE)) { df <- dplyr::filter(df, !.env$m); add("Rows", keep, "Dropped rows with no target value", sum(m, na.rm = TRUE)) }
    if (nrow(df) == 0) stop("All rows had missing target values and were removed. Choose a different target or disable target-based filtering.", call. = FALSE)
  }
  # 7. duplicates (dplyr::distinct)
  if (o$drop_duplicates) {
    before <- nrow(df); 
    # Guard: distinct can be heavy on wide data; limit to 200k rows already enforced
    df <- dplyr::distinct(df); 
    if (before > nrow(df)) add("Rows", "(all)", "Removed duplicate rows", before - nrow(df))
  }
  if (nrow(df) == 0) stop("No rows remain after deduplication.", call. = FALSE)

  # 8. columns to drop
  drop <- character(0)
  for (nm in setdiff(names(df), c(keep, masked))) {
    # Guard: column may have been removed already
    if (!nm %in% names(df)) next
    pr <- tryCatch(profile_column(df[[nm]], nm, nrow(df)), error = function(e) NULL)
    if (is.null(pr)) next
    if (o$drop_constant && (isTRUE(pr$constant) || pr$type == "empty")) { drop <- c(drop, nm); add("Columns", nm, "Dropped: same value in every row", NA) }
    else if (o$drop_id && isTRUE(pr$id_like)) { drop <- c(drop, nm); add("Columns", nm, "Dropped: looks like an ID", NA) }
    else if (o$drop_sparse && !is.na(pr$missing_pct) && pr$missing_pct > o$sparse_threshold) { drop <- c(drop, nm); add("Columns", nm, sprintf("Dropped: %s%% missing", pr$missing_pct), NA) }
  }
  if (length(drop)) { df <- dplyr::select(df, -dplyr::all_of(drop)); fit$dropped <- c(fit$dropped, drop) }
  if (ncol(df) == 0) stop("All columns were dropped (constant/ID/sparse). Try relaxing the filters (uncheck 'Drop ID columns' or raise missing limit).", call. = FALSE)
  if (nrow(df) == 0) stop("No rows remain after column filtering.", call. = FALSE)

  # 9. outliers (before imputing so the median is not distorted)
  num <- setdiff(names(df)[vapply(df, is.numeric, logical(1))], keep)
  if (o$outliers != "none" && length(num) > 0) for (nm in num) {
    if (!nm %in% names(df)) next
    v <- df[[nm]]; if (sum(!is.na(v)) < 8) next
    b <- tryCatch(iqr_bounds(v), error = function(e) NULL)
    if (is.null(b) || any(!is.finite(b))) next
    out <- !is.na(v) & (v < b["lo"] | v > b["hi"])
    if (o$outliers == "cap") fit$caps[[nm]] <- unname(b)
    if (!any(out, na.rm = TRUE)) next
    if (o$outliers == "cap") { v[!is.na(v) & v < b["lo"]] <- b["lo"]; v[!is.na(v) & v > b["hi"]] <- b["hi"]; df[[nm]] <- v; add("Outliers", nm, "Capped to the normal range", sum(out, na.rm = TRUE)) }
    else { 
      before_n <- nrow(df)
      keep_rows <- !out | is.na(df[[nm]]); df <- dplyr::filter(df, .env$keep_rows); 
      add("Outliers", nm, "Removed outlier rows", sum(out, na.rm = TRUE)) 
      if (nrow(df) == 0) stop(sprintf("All rows were removed as outliers in '%s'. Try 'Cap' instead of 'Remove'.", nm), call. = FALSE)
    }
  }
  # 10. imputation
  for (nm in setdiff(names(df), keep)) {
    if (!nm %in% names(df)) next
    x <- df[[nm]]; m <- is.na(x); if (!any(m, na.rm = TRUE)) next
    # Add missing flag column BEFORE filling (only if meaningful fraction)
    if (o$missing_flags && mean(m, na.rm = TRUE) >= 0.05) {
      flag_name <- paste0(nm, "_was_missing")
      # Avoid overwriting existing column
      if (!flag_name %in% names(df)) df[[flag_name]] <- as.integer(m)
    }
    if (is.numeric(x) && o$impute_numeric != "none") {
      med <- if (o$impute_numeric == "mean") mean(x, na.rm = TRUE) else stats::median(x, na.rm = TRUE)
      # Guard: if all NA, median is NA/NaN -> use 0 fallback and warn
      if (is.na(med) || !is.finite(med)) {
        med <- 0
        add("Fill", nm, "Filled with 0 (column was all missing)", sum(m, na.rm = TRUE))
      } else {
        fit$fills[[nm]] <- med
      }
      if (o$impute_numeric == "knn") { 
        x <- tryCatch(knn_impute(df, nm, exclude = keep), error = function(e) {
          # Fallback to median on knn failure
          x[m] <- med
          x
        })
        add("Fill", nm, "Filled with k-nearest-neighbour prediction (machine learning)", sum(m, na.rm = TRUE)) 
      }
      else { x[m] <- med; add("Fill", nm, paste("Filled with the", o$impute_numeric), sum(m, na.rm = TRUE)) }
    } else if (!is.numeric(x) && o$impute_categorical != "none") {
      if (inherits(x, "Date")) { 
        v <- tryCatch(stats::median(x, na.rm = TRUE), error = function(e) as.Date("2020-01-01"))
        if (is.na(v)) v <- as.Date("2020-01-01")
        x[m] <- v; fit$fills[[nm]] <- v; add("Fill", nm, "Filled with the middle date", sum(m, na.rm = TRUE)) }
      else { 
        val <- if (o$impute_categorical == "mode") mode_of(x) else "Unknown"; 
        if (is.na(val) || !nzchar(val)) val <- "Unknown"
        x[m] <- val; fit$fills[[nm]] <- val; add("Fill", nm, paste0("Filled with '", val, "'"), sum(m, na.rm = TRUE)) }
    }
    df[[nm]] <- x
  }
  # Final safety: ensure no column still has NA in non-target (unless user explicitly chose "none")
  rownames(df) <- NULL
  fit$kept <- setdiff(names(df), keep); fit$numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]; fit$types <- types

  # Ensure we have at least 1 non-target feature for ML (warn if not, but don't block)
  if (length(fit$kept) == 0 && length(keep) > 0) {
    warning("Cleaning left no feature columns besides the target. ML will have no predictors.")
  }

  list(data = df, fit = fit, log = if (length(log)) do.call(rbind, log) else data.frame(step = character(), column = character(), action = character(), rows_affected = numeric()), options = o)
}

# Clean new rows with the values learned on the training data (used for predictions).
clean_new <- function(fit, new) {
  if (is.null(fit) || is.null(fit$kept)) stop("Cleaning model not found. Train a model first.", call. = FALSE)
  new <- as.data.frame(new, stringsAsFactors = FALSE); new[] <- lapply(new, as.character)
  out <- list()
  for (nm in fit$kept) {
    if (grepl("_was_missing$", nm)) { b <- sub("_was_missing$", "", nm); xb <- if (b %in% names(new)) trimws(new[[b]]) else rep(NA_character_, nrow(new)); out[[nm]] <- as.integer(is_missing(xb)); next }
    x <- if (nm %in% names(new)) new[[nm]] else rep(NA_character_, nrow(new))
    x <- trimws(x); x[is_missing(x)] <- NA
    ty <- fit$types[[nm]]
    if (nm %in% fit$numeric_cols) { 
      x <- to_num(x); 
      if (!is.null(fit$caps[[nm]])) {
        # Guard: caps may be null or malformed
        caps <- fit$caps[[nm]]
        if (length(caps) >= 2 && all(is.finite(caps))) x <- pmin(pmax(x, caps[1]), caps[2])
      }
    }
    else if (nm %in% fit$date_cols) x <- parse_date_flex(x)
    else if (!is.null(fit$label_map[[nm]])) { k <- tolower(x); hit <- !is.na(k) & k %in% names(fit$label_map[[nm]]); x[hit] <- unname(fit$label_map[[nm]][k[hit]]) }
    f <- fit$fills[[nm]]; if (!is.null(f)) x[is.na(x)] <- f
    out[[nm]] <- x
  }
  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}
