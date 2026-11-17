# Export (Unit III): CSV, Excel workbook, standalone HTML report, JSON recipe and a ZIP bundle.
esc <- function(x) htmltools::htmlEscape(as.character(x))

fmt_cols <- function(prof) {
  cl <- prof$columns
  data.frame(Column = cl$name, Type = cl$type, `Missing %` = cl$missing_pct, Unique = cl$unique_n,
             Outliers = cl$outlier_n, Skew = cl$skew, Example = cl$example, check.names = FALSE, stringsAsFactors = FALSE)
}

build_excel <- function(path, ctx) {
  sheets <- list(
    Summary = data.frame(Item = c("File", "Rows before", "Rows after", "Columns before", "Columns after", "Score before", "Score after", "Grade after", "Target", "Created by"),
      Value = c(ctx$name, ctx$prof_before$overview$rows, nrow(ctx$clean$data), ctx$prof_before$overview$cols, ncol(ctx$clean$data),
                ctx$score_before$score, ctx$score_after$score, ctx$score_after$grade, ifelse(nzchar(ctx$target), ctx$target, "(none)"),
                paste(APP_NAME, APP_VERSION)), stringsAsFactors = FALSE),
    `Issues found` = ctx$score_before$issues,
    `Score breakdown` = merge_breakdown(ctx$score_before, ctx$score_after),
    `Column profile before` = fmt_cols(ctx$prof_before),
    `Column profile after` = fmt_cols(ctx$prof_after),
    `Cleaning log` = ctx$clean$log
  )
  if (!is.null(ctx$mod)) { sheets[["Model results"]] <- ctx$mod$results[, setdiff(names(ctx$mod$results), "main_metric")]; if (!is.null(ctx$mod$importance)) sheets[["Feature importance"]] <- ctx$mod$importance }
  sheets[["Cleaned data"]] <- ctx$clean$data
  writexl::write_xlsx(sheets, path)
  invisible(path)
}

merge_breakdown <- function(b, a) {
  data.frame(Category = b$breakdown$category, `Max points` = b$breakdown$max, `Lost before` = b$breakdown$lost,
             `Lost after` = a$breakdown$lost, check.names = FALSE, stringsAsFactors = FALSE)
}

html_table <- function(df, max_rows = 200) {
  if (!nrow(df)) return("<p class='muted'>Nothing to show.</p>")
  df <- utils::head(df, max_rows)
  head <- paste0("<th>", esc(names(df)), "</th>", collapse = "")
  body <- paste0(vapply(seq_len(nrow(df)), function(i) paste0("<tr>", paste0("<td>", esc(unlist(df[i, ], use.names = FALSE)), "</td>", collapse = ""), "</tr>"), ""), collapse = "")
  sprintf("<div class='scroll'><table><thead><tr>%s</tr></thead><tbody>%s</tbody></table></div>", head, body)
}

build_html_report <- function(ctx) {
  sb <- ctx$score_before; sa <- ctx$score_after
  css <- "body{font-family:Segoe UI,Helvetica,Arial,sans-serif;color:#1c2b33;max-width:960px;margin:0 auto;padding:24px;line-height:1.5}
  h1{margin:0}h2{margin-top:32px;border-bottom:2px solid #0f5c4d;padding-bottom:4px}.muted{color:#5b6b73}
  .scores{display:flex;gap:16px;flex-wrap:wrap;margin:16px 0}.card{border:1px solid #cfd9d6;border-radius:8px;padding:16px 20px;min-width:180px}
  .big{font-size:44px;font-weight:700;color:#0f5c4d}.bad{color:#b5392f}.scroll{overflow-x:auto}
  table{border-collapse:collapse;width:100%;font-size:13px}th,td{border:1px solid #dfe7e4;padding:6px 8px;text-align:left}th{background:#eef3f1}
  footer{margin-top:40px;font-size:12px;color:#5b6b73}@media print{body{padding:0}}"
  sprintf("<!doctype html><html lang='en'><head><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'><title>%s data readiness report</title><style>%s</style></head><body>
  <h1>Data readiness report</h1><p class='muted'>%s &middot; generated %s by %s %s</p>
  <div class='scores'><div class='card'><div class='muted'>Score before</div><div class='big bad'>%d</div>%s</div>
  <div class='card'><div class='muted'>Score after cleaning</div><div class='big'>%d</div>%s</div>
  <div class='card'><div class='muted'>Rows</div><div class='big' style='font-size:28px'>%s &rarr; %s</div>Columns %d &rarr; %d</div></div>
  <h2>Issues found</h2>%s<h2>What was changed</h2>%s<h2>Score breakdown</h2>%s<h2>Machine learning preparation</h2><ul>%s</ul>%s
  <footer>Report created with %s by %s. Scores are guidance for ML readiness, not a guarantee of model quality.</footer></body></html>",
    esc(APP_NAME), css, esc(ctx$name), format(Sys.time(), "%d %b %Y %H:%M"), esc(APP_NAME), esc(APP_VERSION),
    sb$score, esc(sb$grade), sa$score, esc(sa$grade),
    format(ctx$prof_before$overview$rows, big.mark = ","), format(nrow(ctx$clean$data), big.mark = ","),
    ctx$prof_before$overview$cols, ncol(ctx$clean$data),
    html_table(sb$issues[, c("category", "column", "reason", "fix", "points")]), html_table(ctx$clean$log),
    html_table(merge_breakdown(sb, sa)),
    paste0("<li>", esc(if (is.null(ctx$ml)) "Not prepared yet" else ctx$ml$steps), "</li>", collapse = ""), model_html(ctx), esc(APP_NAME), esc(AUTHOR$name))
}

model_html <- function(ctx) {
  if (is.null(ctx$mod)) return("")
  m <- ctx$mod; r <- m$results; r[] <- lapply(r, function(x) if (is.numeric(x)) round(x, 3) else x); r$main_metric <- NULL
  cv <- if (!is.null(m$cv)) sprintf("<p>Cross-validated %s of the best model: %.3f &plusmn; %.3f (%d folds).</p>", esc(m$cv$metric), m$cv$mean, m$cv$sd, m$cv$folds) else ""
  imp <- if (!is.null(m$importance)) paste0("<h3>Most important features</h3>", html_table(utils::head(m$importance[, c("feature", "share")], 10))) else ""
  rc <- if (!is.null(ctx$rawcmp) && isTRUE(ctx$rawcmp$ok)) sprintf("<p>Same random forest on the raw file: %s %.3f. After cleaning: %.3f.</p>", esc(ctx$rawcmp$metric), ctx$rawcmp$raw, ctx$rawcmp$clean) else ""
  paste0("<h2>Models</h2><p>Best model: <b>", esc(m$best), "</b> (", esc(m$results$main_metric[1]), " ", sprintf("%.3f", m$results$main[1]), ").</p>", cv, rc, html_table(r), imp)
}

build_recipe <- function(ctx) {
  jsonlite::toJSON(list(tool = APP_NAME, version = APP_VERSION, file = ctx$name, target = ctx$target,
    cleaning = ctx$clean$options, ml = if (is.null(ctx$ml)) NULL else ctx$ml$options,
    score_before = ctx$score_before$score, score_after = ctx$score_after$score,
    best_model = if (is.null(ctx$mod)) NULL else ctx$mod$best, scaling_parameters = if (is.null(ctx$ml)) NULL else ctx$ml$scale_params), auto_unbox = TRUE, pretty = TRUE, null = "null")
}

python_snippet <- function(ctx) {
  tg <- if (nzchar(ctx$target)) ctx$target else "target"
  paste0("# Load the files exported by ", APP_NAME, "\nimport pandas as pd\n\ntrain = pd.read_csv('train.csv')\ntest = pd.read_csv('test.csv')\n",
         "X_train, y_train = train.drop(columns=['", tg, "']), train['", tg, "']\nX_test, y_test = test.drop(columns=['", tg, "']), test['", tg, "']\n\n",
         "from sklearn.ensemble import RandomForestClassifier   # use RandomForestRegressor for numeric targets\n",
         "model = RandomForestClassifier(random_state=42).fit(X_train, y_train)\nprint('Test accuracy:', model.score(X_test, y_test))\n")
}

build_bundle <- function(zip_path, ctx) {
  # Use random suffix to avoid collisions when multiple users export simultaneously
  d <- file.path(tempdir(), paste0("bundle_", as.integer(Sys.time()), "_", paste0(sample(c(letters, 0:9), 6, replace = TRUE), collapse=""))); dir.create(d, showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(ctx$clean$data, file.path(d, "cleaned_data.csv"), row.names = FALSE)
  if (!is.null(ctx$ml)) {
    utils::write.csv(ctx$ml$train, file.path(d, "train.csv"), row.names = FALSE)
    utils::write.csv(ctx$ml$test, file.path(d, "test.csv"), row.names = FALSE)
    writeLines(python_snippet(ctx), file.path(d, "load_in_python.py"))
  }
  if (!is.null(ctx$bundle)) saveRDS(ctx$bundle, file.path(d, "model.rds"))
  if (!is.null(ctx$mod)) { utils::write.csv(ctx$mod$preds[[ctx$mod$best]], file.path(d, "test_predictions.csv"), row.names = FALSE); utils::write.csv(ctx$mod$results, file.path(d, "model_results.csv"), row.names = FALSE) }
  build_excel(file.path(d, "rinse_report.xlsx"), ctx)
  writeLines(build_html_report(ctx), file.path(d, "rinse_report.html"))
  writeLines(build_recipe(ctx), file.path(d, "recipe.json"))
  zip::zipr(zip_path, list.files(d, full.names = TRUE), mode = "cherry-pick")
  invisible(zip_path)
}

# Export to a database: cleaned data, the cleaning log and (if prepared) the train and test sets as tables in one SQLite file (Unit III: databases).
build_sqlite <- function(path, ctx) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path); on.exit(DBI::dbDisconnect(con))
  as_plain <- function(d) { d <- as.data.frame(d, stringsAsFactors = FALSE); d[] <- lapply(d, function(x) if (inherits(x, "Date")) as.character(x) else x); d }
  DBI::dbWriteTable(con, "cleaned_data", as_plain(ctx$clean$data), overwrite = TRUE)
  DBI::dbWriteTable(con, "cleaning_log", as_plain(ctx$clean$log), overwrite = TRUE)
  if (!is.null(ctx$ml)) {
    DBI::dbWriteTable(con, "train_set", as_plain(ctx$ml$train), overwrite = TRUE)
    DBI::dbWriteTable(con, "test_set", as_plain(ctx$ml$test), overwrite = TRUE)
  }
  invisible(path)
}
