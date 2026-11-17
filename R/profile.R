# Profiling engine (Unit II: vectors, data frames, factors, built-in functions).

is_missing <- function(x) {
  if (is.character(x) || is.factor(x)) {
    x <- as.character(x)
    is.na(x) | tolower(trimws(x)) %in% MISSING_TOKENS
  } else is.na(x)
}

# Convert text such as "₹1,200", "45%", "$ 3.5" to numbers. Anything unparseable becomes NA.
to_num <- function(x) {
  x <- as.character(x)
  x <- gsub("[\u20b9$\u20ac\u00a3,%\\s]|rs\\.?|inr|usd", "", tolower(x), perl = TRUE)
  x[x == ""] <- NA
  suppressWarnings(as.numeric(x))
}

DATE_PATTERNS <- c("^\\d{4}-\\d{1,2}-\\d{1,2}$", "^\\d{1,2}/\\d{1,2}/\\d{2,4}$", "^\\d{1,2}-\\d{1,2}-\\d{2,4}$",
                   "^\\d{4}/\\d{1,2}/\\d{1,2}$", "^\\d{1,2}\\.\\d{1,2}\\.\\d{4}$")
looks_date <- function(v) {
  v <- trimws(v)
  hit <- Reduce(`|`, lapply(DATE_PATTERNS, function(p) grepl(p, v)))
  mean(hit)
}

parse_date_flex <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(as.Date(NA), length(x))
  fmts <- c("%Y-%m-%d", "%d/%m/%Y", "%d-%m-%Y", "%Y/%m/%d", "%d.%m.%Y", "%d/%m/%y", "%d-%m-%y")
  for (f in fmts) {
    todo <- is.na(out) & !is.na(x)
    if (!any(todo)) break
    out[todo] <- suppressWarnings(as.Date(x[todo], format = f))
  }
  out
}

skewness <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3 || stats::sd(x) == 0) return(0)
  mean((x - mean(x))^3) / stats::sd(x)^3
}

iqr_bounds <- function(x) {
  q <- stats::quantile(x, c(.25, .75), na.rm = TRUE, names = FALSE)
  i <- q[2] - q[1]
  c(lo = q[1] - 1.5 * i, hi = q[2] + 1.5 * i)
}

BOOL_SET <- c("yes", "no", "y", "n", "true", "false", "t", "f", "0", "1")

# Infer a semantic type for one column.
infer_type <- function(x, name = "") {
  if (inherits(x, "Date")) return(list(type = "date", num_text = FALSE))
  if (is.logical(x)) return(list(type = "boolean", num_text = FALSE))
  if (is.numeric(x)) return(list(type = "numeric", num_text = FALSE))
  if (is.factor(x)) x <- as.character(x)
  v <- x[!is_missing(x)]
  if (!length(v)) return(list(type = "empty", num_text = FALSE))
  u <- unique(tolower(trimws(v)))
  if (length(u) <= 3 && all(u %in% BOOL_SET) && length(u) >= 2 && !all(u %in% c("0", "1")))
    return(list(type = "boolean", num_text = FALSE))
  nums <- to_num(v)
  if (mean(!is.na(nums)) >= 0.9) {
    sym <- any(grepl("[^0-9.\\-\\s+eE]", v[!is.na(nums)], perl = TRUE))
    return(list(type = "numeric", num_text = sym))
  }
  if (looks_date(v) >= 0.9) return(list(type = "date", num_text = FALSE))
  ur <- length(unique(v)) / length(v)
  if (length(unique(v)) <= 50 || ur < 0.5) return(list(type = "categorical", num_text = FALSE))
  list(type = "text", num_text = FALSE)
}

looks_like_id <- function(x, name, type, n) {
  nm <- grepl("(^|[_ .-])(id|uuid|guid|index|sr|sno|s\\.no|serial|roll|enrol)([_ .-]|$)|^id$|id$", tolower(name))
  ux <- length(unique(x[!is_missing(x)]))
  (nm && ux >= 0.9 * n) || (type %in% c("text") && ux == n)
}


# ---- Personal data (PII) detection ----
PII_NAME_HINT <- "(^|[_ .-])(e-?mail|phone|mobile|contact|aadhaar|aadhar|pan|ssn|passport|address|dob|birth|first_?name|last_?name|full_?name|student_?name|customer_?name|name)([_ .-]|$)"
detect_pii <- function(x, name) {
  v <- as.character(x[!is_missing(x)]); if (length(v) < 3) return("")
  v <- utils::head(v, 500); h <- function(re, z = v) mean(grepl(re, z))
  vc <- gsub("[ -]", "", v)
  if (h("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$") > 0.6) return("email")
  if (h("^(\\+91|91|0)?[6-9][0-9]{9}$", vc) > 0.6) return("phone number")
  if (h("^[2-9][0-9]{11}$", vc) > 0.6 || h("^[A-Z]{5}[0-9]{4}[A-Z]$", vc) > 0.6) return("national ID")
  if (grepl(PII_NAME_HINT, tolower(name)) && !grepl("(^|[_ .-])(branch|city|device|merchant|plan|file|product|company|hospital|school|college)", tolower(name))) {
    if (grepl("addr", tolower(name))) return("address")
    if (grepl("name", tolower(name)) && length(unique(v)) / length(v) > 0.5) return("name")
  }
  ""
}

# ---- Target leakage: a feature that is almost a copy of the target ----
assoc_strength <- function(x, xtype, y, ytype) {
  ok <- !is.na(x) & !is.na(y); x <- x[ok]; y <- y[ok]; if (length(x) < 30) return(0)
  if (ytype == "regression") {
    if (xtype == "numeric") return(abs(suppressWarnings(stats::cor(x, y))) %||% 0)
    g <- factor(x); if (nlevels(g) < 2 || nlevels(g) > 50) return(0)
    return(sqrt(max(0, summary(stats::lm(y ~ g))$r.squared)))
  }
  if (xtype == "numeric") {
    g <- factor(y); if (nlevels(g) < 2) return(0)
    return(sqrt(max(0, summary(stats::lm(x ~ g))$r.squared)))
  }
  tb <- table(x, y); if (nrow(tb) < 2 || ncol(tb) < 2 || nrow(tb) > 50) return(0)
  ch <- suppressWarnings(stats::chisq.test(tb, correct = FALSE)$statistic)
  sqrt(as.numeric(ch) / (sum(tb) * (min(dim(tb)) - 1)))
}

leakage_suspects <- function(df, target, types, max_rows = 4000) {
  if (is.null(target) || !nzchar(target) || !target %in% names(df)) return(data.frame(column = character(), strength = numeric()))
  if (nrow(df) > max_rows) df <- df[sample(nrow(df), max_rows), , drop = FALSE]
  y0 <- df[[target]]; ym <- is_missing(y0); df <- df[!ym, , drop = FALSE]; y0 <- y0[!ym]
  ty <- types[[target]]
  ytype <- if (ty %in% c("categorical", "boolean") || length(unique(y0)) <= 10) "classification" else "regression"
  y <- if (ytype == "regression") (if (is.numeric(y0)) y0 else to_num(y0)) else as.character(y0)
  out <- list()
  for (nm in setdiff(names(df), target)) {
    xt <- types[[nm]]; if (!xt %in% c("numeric", "categorical", "boolean")) next
    x0 <- df[[nm]]; if (length(unique(x0[!is_missing(x0)])) >= 0.95 * nrow(df)) next    # IDs are handled elsewhere
    x <- if (xt == "numeric") (if (is.numeric(x0)) x0 else to_num(x0)) else { z <- as.character(x0); z[is_missing(z)] <- NA; z }
    st <- tryCatch(assoc_strength(x, if (xt == "numeric") "numeric" else "cat", y, ytype), error = function(e) 0)
    if (is.finite(st) && st >= 0.95) out[[length(out) + 1]] <- data.frame(column = nm, strength = round(st, 3))
  }
  if (length(out)) do.call(rbind, out) else data.frame(column = character(), strength = numeric())
}

profile_column <- function(x, name, n) {
  ty <- infer_type(x, name)
  type <- ty$type
  miss <- is_missing(x)
  v <- x[!miss]
  res <- list(name = name, type = type, missing_n = sum(miss), missing_pct = round(100 * mean(miss), 1),
              unique_n = length(unique(v)), example = if (length(v)) as.character(v[1]) else "",
              constant = length(unique(v)) <= 1, id_like = FALSE, num_text = ty$num_text,
              outlier_n = 0L, outlier_pct = 0, skew = 0, ws_issue = FALSE, case_issue = FALSE,
              disguised_missing = 0L, pii = "", mean = NA_real_, median = NA_real_, min = NA_real_, max = NA_real_)
  if (is.character(x) || is.factor(x)) {
    xc <- as.character(x)
    res$disguised_missing <- sum(!is.na(xc) & is_missing(xc) & trimws(xc) != "" & tolower(trimws(xc)) %in% MISSING_TOKENS)
    res$ws_issue <- any(xc[!miss] != trimws(xc[!miss]))
  }
  if (type == "categorical" && length(v)) {
    vv <- trimws(as.character(v))
    res$case_issue <- length(unique(vv)) > length(unique(tolower(vv)))
  }
  if (type == "numeric" && length(v)) {
    nv <- if (is.numeric(x)) as.numeric(v) else to_num(v)
    nv <- nv[is.finite(nv)]
    if (length(nv) > 3) {
      b <- iqr_bounds(nv); o <- nv < b["lo"] | nv > b["hi"]
      res$outlier_n <- sum(o); res$outlier_pct <- round(100 * mean(o), 1)
      res$skew <- round(skewness(nv), 2)
      res$mean <- mean(nv); res$median <- stats::median(nv); res$min <- min(nv); res$max <- max(nv)
    }
  }
  res$id_like <- looks_like_id(v, name, type, max(1, length(v)))
  if (type %in% c("numeric", "text", "categorical")) res$pii <- detect_pii(x, name)
  res
}

# Main profiler. `target` is optional.
profile_data <- function(df, target = NULL) {
  n <- nrow(df)
  cols <- lapply(names(df), function(nm) profile_column(df[[nm]], nm, n))
  # Handle edge of 0-row data gracefully instead of crashing
  if (nrow(df) == 0) {
    ctab <- do.call(rbind, lapply(cols, function(c) as.data.frame(c, stringsAsFactors = FALSE)))
    return(list(leakage = data.frame(column=character(), strength=numeric()),
                overview = list(rows=0, cols=ncol(df), duplicates=0, missing_cells=0, missing_pct=0),
                columns = ctab, target = NULL))
  }
  ctab <- do.call(rbind, lapply(cols, function(c) as.data.frame(c, stringsAsFactors = FALSE)))
  dup_n <- tryCatch(sum(duplicated(df)), error = function(e) 0)
  miss_cells <- sum(vapply(df, function(x) sum(is_missing(x)), numeric(1)))
  tgt <- NULL
  if (!is.null(target) && nzchar(target) && target %in% names(df)) {
    y <- df[[target]]; y <- y[!is_missing(y)]
    ty <- ctab$type[ctab$name == target]
    if (ty %in% c("categorical", "boolean") || length(unique(y)) <= 10) {
      tb <- sort(table(y), decreasing = TRUE)
      tgt <- list(name = target, task = "classification", classes = as.list(tb),
                  majority_share = round(100 * max(tb) / sum(tb), 1),
                  minority_share = round(100 * min(tb) / sum(tb), 1),
                  missing_n = sum(is_missing(df[[target]])))
    } else {
      tgt <- list(name = target, task = "regression", missing_n = sum(is_missing(df[[target]])),
                  skew = round(skewness(to_num(y)), 2))
    }
  }
  leak <- leakage_suspects(df, target, stats::setNames(ctab$type, ctab$name))
  list(leakage = leak, overview = list(rows = n, cols = ncol(df), duplicates = dup_n,
                       missing_cells = miss_cells,
                       missing_pct = round(100 * miss_cells / (n * ncol(df)), 2)),
       columns = ctab, target = tgt)
}
