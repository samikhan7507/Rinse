# ML-Readiness Score: 100 points, minus itemised deductions. Every deduction has a reason.
SCORE_CAPS <- c(Completeness = 30, Uniqueness = 15, Consistency = 20, Usefulness = 15, Distribution = 10, Target = 10)

ded <- function(cat, pts, col, reason, fix) {
  data.frame(category = cat, points = round(pts, 1), column = col, reason = reason, fix = fix, stringsAsFactors = FALSE)
}

score_data <- function(prof) {
  ov <- prof$overview; cl <- prof$columns; rows <- list()
  for (i in seq_len(nrow(cl))) {
    c <- cl[i, ]
    if (c$missing_n > 0 && c$type != "empty")
      rows[[length(rows) + 1]] <- ded("Completeness", min(6, c$missing_pct / 100 * 30), c$name,
        sprintf("%s%% of values are missing", c$missing_pct),
        if (c$missing_pct > 60) "Drop this column" else if (c$type == "numeric") "Fill with the median" else "Fill with the most common value")
    if (c$type == "empty")
      rows[[length(rows) + 1]] <- ded("Usefulness", 3, c$name, "Column is completely empty", "Drop this column")
    if (isTRUE(c$ws_issue))
      rows[[length(rows) + 1]] <- ded("Consistency", 1.5, c$name, "Extra spaces before or after values", "Trim spaces")
    if (isTRUE(c$case_issue))
      rows[[length(rows) + 1]] <- ded("Consistency", 3, c$name, "Same label written in different cases (e.g. Male / male)", "Unify labels")
    if (c$disguised_missing > 0)
      rows[[length(rows) + 1]] <- ded("Consistency", 1.5, c$name, sprintf("%d placeholders such as N/A, ?, or - used instead of blanks", c$disguised_missing), "Convert placeholders to missing")
    if (isTRUE(c$num_text))
      rows[[length(rows) + 1]] <- ded("Consistency", 3, c$name, "Numbers stored as text with symbols (\u20b9, $, commas, %)", "Convert to numbers")
    if (nzchar(c$pii))
      rows[[length(rows) + 1]] <- ded("Usefulness", 3, c$name, sprintf("Contains personal data (%s). It should not be used to train a model", c$pii), "Drop or mask this column")
    if (c$constant && c$type != "empty")
      rows[[length(rows) + 1]] <- ded("Usefulness", 3, c$name, "Every row has the same value", "Drop this column")
    if (isTRUE(c$id_like) && !c$constant)
      rows[[length(rows) + 1]] <- ded("Usefulness", 2, c$name, "Looks like an ID. It carries no pattern for a model", "Drop this column")
    if (c$type == "text" && !isTRUE(c$id_like))
      rows[[length(rows) + 1]] <- ded("Usefulness", 2, c$name, "Free text with too many unique values for simple encoding", "Drop or process separately")
    if (c$type == "numeric" && c$outlier_pct > 2)
      rows[[length(rows) + 1]] <- ded("Distribution", min(2.5, c$outlier_pct / 4), c$name, sprintf("%s%% of values are outliers", c$outlier_pct), "Cap at the normal range")
    if (c$type == "numeric" && abs(c$skew) > 1.5)
      rows[[length(rows) + 1]] <- ded("Distribution", 1, c$name, sprintf("Strongly skewed (skewness %.1f)", c$skew), "Consider a log transform")
  }
  if (ov$duplicates > 0) {
    dp <- 100 * ov$duplicates / ov$rows
    rows[[length(rows) + 1]] <- ded("Uniqueness", min(15, dp * 1.5 + 1), "(all rows)", sprintf("%d duplicate rows (%.1f%%)", ov$duplicates, dp), "Remove duplicates")
  }
  lk <- prof$leakage
  if (!is.null(lk) && nrow(lk)) for (i in seq_len(nrow(lk)))
    rows[[length(rows) + 1]] <- ded("Usefulness", 5, lk$column[i], sprintf("Almost identical to the target (strength %.2f). It probably leaks the answer", lk$strength[i]), "Drop this column")
  t <- prof$target
  if (!is.null(t)) {
    if (t$missing_n > 0)
      rows[[length(rows) + 1]] <- ded("Target", min(5, 100 * t$missing_n / ov$rows), t$name, sprintf("%d rows have no target value", t$missing_n), "Drop those rows")
    if (t$task == "classification" && t$majority_share > 70)
      rows[[length(rows) + 1]] <- ded("Target", min(5, (t$majority_share - 70) / 6), t$name,
        sprintf("Imbalanced classes: the largest class is %s%% of rows", t$majority_share), "Use stratified split and class weights")
  }
  d <- if (length(rows)) do.call(rbind, rows) else ded("Completeness", 0, "", "", "")[0, ]
  # cap per category
  lost_by_cat <- d |> dplyr::group_by(category) |> dplyr::summarise(pts = sum(points), .groups = "drop")
  cat_tot <- vapply(names(SCORE_CAPS), function(k) { s <- lost_by_cat$pts[lost_by_cat$category == k]; min(SCORE_CAPS[[k]], if (length(s)) s else 0) }, numeric(1))
  score <- max(0, round(100 - sum(cat_tot)))
  d <- dplyr::arrange(d, dplyr::desc(points))
  list(score = score, grade = grade_of(score), breakdown = data.frame(category = names(SCORE_CAPS), max = as.numeric(SCORE_CAPS),
         lost = round(as.numeric(cat_tot), 1), stringsAsFactors = FALSE), issues = d)
}

grade_of <- function(s) if (s >= 85) "Ready" else if (s >= 70) "Almost ready" else if (s >= 50) "Needs work" else "Not ready"
