# Screens for machine learning: model comparison, insights, prediction on new data, drift check.
register_ml <- function(input, output, session, rv, safe, V) {

  # ---------- Step 5: models ----------
  observeEvent(input$run_models, {
    req(rv$ml, rv$ml$task != "none")
    sel <- input$mod_sel; if (!length(sel)) { showNotification("Select at least one model.", type = "error"); return() }
    rv$inp$mod_sel <- sel; rv$inp$cv <- input$mod_cv
    withProgress(message = "Training models", value = 0, safe({
      mod <- train_models(rv$ml, models = sel, seed = rv$ml$options$seed, cv_folds = as.integer(input$mod_cv %||% 5),
                          progress = function(i, n, nm) setProgress(value = (i - 1) / n, detail = nm))
      rv$mod <- mod; rv$bundle <- make_bundle(rv$clean, rv$ml, mod, mod$best); rv$rawcmp <- NULL
      save_model_runs(rv$run_id, rv$user$id, rv$name, mod$results, mod$task)
      audit(rv$user$id, "train_models", sprintf("%s best=%s %.3f", rv$name, mod$best, mod$results$main[1]))
      vlog("INFO", "Trained %d models on %s, best %s (%.3f)", nrow(mod$results), rv$name, mod$best, mod$results$main[1])
    }))
  })
  observeEvent(input$run_rawcmp, {
    req(rv$mod, rv$clean)
    withProgress(message = "Training on the raw file", value = .5, safe({
      rf <- rv$mod$results$main[rv$mod$results$model == "Random forest"]; cm <- if (length(rf)) rf[1] else rv$mod$results$main[1]
      rv$rawcmp <- raw_baseline(rv$raw, rv$target, rv$prof0, rv$ml, cm, rv$ml$options$seed)
    }))
  })

  fmt_results <- function(res) {
    if ("accuracy" %in% names(res)) data.frame(Model = res$model, Accuracy = round(res$accuracy, 3), `Balanced accuracy` = round(res$balanced, 3), `F1 (macro)` = round(res$f1, 3), `ROC AUC` = round(res$auc, 3), Seconds = round(res$seconds, 2), check.names = FALSE)
    else data.frame(Model = res$model, `R squared` = round(res$r2, 3), RMSE = round(res$rmse, 3), MAE = round(res$mae, 3), Seconds = round(res$seconds, 2), check.names = FALSE)
  }

  stage_models <- function() {
    if (is.null(rv$ml)) return(div(class = "card empty", h3("Prepare your data first"), p("Finish step 4, then come back to train models.")))
    if (rv$ml$task == "none") return(div(class = "card empty", h3("No target column chosen"), p("Models need something to predict. Go to step 1, pick a target column, and run the steps again."), actionButton("next_step", "Skip to export", class = "btn")))
    cls <- rv$ml$task == "classification"; all_m <- if (cls) MODELS_CLS else MODELS_REG
    form <- div(class = "card", h3("5. Train and compare models"),
      p(class = "muted", sprintf("Every model learns from the same %s training rows and is judged on the same %s unseen test rows.", format(nrow(rv$ml$train), big.mark = ","), format(nrow(rv$ml$test), big.mark = ","))),
      checkboxGroupInput("mod_sel", "Models", choices = all_m, selected = rv$inp$mod_sel %||% all_m),
      selectInput("mod_cv", "Cross-validation of the best model", c("5 folds" = 5, "3 folds" = 3, "Off" = 0), rv$inp$cv %||% 5),
      actionButton("run_models", if (is.null(rv$mod)) "Train models" else "Train again", class = "btn primary"))
    if (is.null(rv$mod)) return(form)
    m <- rv$mod; r <- m$results; b <- r[1, ]; cvt <- if (!is.null(m$cv)) sprintf(" Cross-validated %s: %.3f \u00b1 %.3f over %d folds.", m$cv$metric, m$cv$mean, m$cv$sd, m$cv$folds) else ""
    headline <- if (m$task == "classification") sprintf("%s reached %.1f%% accuracy and a macro F1 of %.3f on rows it never saw. %s: %.1f%%.%s", b$model, 100 * b$accuracy, b$f1, m$baseline$label, 100 * m$baseline$value, cvt)
                else sprintf("%s explains %.1f%% of the variation in %s on unseen rows (RMSE %.3f).%s", b$model, 100 * b$r2, m$target, b$rmse, cvt)
    if (m$task == "classification" && b$accuracy <= m$baseline$value + 0.005) headline <- paste(headline, "Accuracy alone does not beat guessing here because one class dominates, which is why Rinse ranks models by macro F1.")
    aucs <- if (m$task == "classification" && "auc" %in% names(r)) stats::setNames(r$auc, r$model) else NULL
    cm_sel <- if (length(m$cms)) div(selectInput("cm_model", "Confusion matrix for", names(m$cms), input$cm_model %||% m$best), uiOutput("cm_view")) else NULL
    rawcard <- div(class = "card", h3("Did cleaning help?"), p(class = "muted", "Rinse trains the same random forest on your raw file the way a beginner would (delete rows with gaps, convert text to numbers) and compares."),
      if (is.null(rv$rawcmp)) actionButton("run_rawcmp", "Run the raw vs clean comparison", class = "btn")
      else if (!isTRUE(rv$rawcmp$ok)) div(class = "msg info", rv$rawcmp$msg)
      else { x <- rv$rawcmp; lk <- rv$prof0$leakage; inflated <- !is.null(lk) && nrow(lk) > 0 && x$raw > x$clean
        tagList(if (inflated) div(class = "msg info", sprintf("The raw score is higher only because the raw file still contains %s, which gives the answer away. A model that leans on it looks brilliant in testing and fails on new data. Rinse removed it, so the cleaned score is the honest one.", paste(lk$column, collapse = ", "))),
        div(class = "grid3", stat_box(paste(x$metric, "on raw data"), sprintf("%.3f", x$raw), if (!inflated && x$clean > x$raw) "bad"), stat_box(paste(x$metric, "after Rinse"), sprintf("%.3f", x$clean), if (!inflated && x$clean >= x$raw) "good"), stat_box("Rows used", sprintf("%s vs %s", format(x$rows_kept, big.mark = ","), format(x$rows_total, big.mark = ",")))),
        p(class = "small muted", style = "margin-top:10px", sprintf("The raw baseline could use only %s of %s rows because deleting every row with a gap throws data away. Rinse keeps all rows by filling gaps sensibly.", format(x$rows_kept, big.mark = ","), format(x$rows_total, big.mark = ",")))) })
    tagList(form,
      div(class = "card", h3("Result"), div(class = "grid3", stat_box("Best model", b$model, "good"), stat_box(b$main_metric, sprintf("%.3f", b$main)), stat_box("Models compared", nrow(r))), p(style = "margin-top:12px", headline),
          tbl_ui(fmt_results(r), 10), if (length(m$notes)) div(class = "msg info", style = "margin-top:10px", paste(m$notes, collapse = " "))),
      div(class = "grid2", div(class = "card", h3(paste("Model comparison:", b$main_metric)), hbars(r$model, pmax(0, r$main), vmax = max(1, max(r$main)), fmt = function(v) sprintf("%.3f", v))),
          div(class = "card", h3(if (length(m$rocs)) "ROC curves" else "Feature importance"), if (length(m$rocs)) svg_roc(m$rocs, aucs) else if (!is.null(m$importance)) hbars(utils::head(m$importance$feature, 10), utils::head(m$importance$share, 10), fmt = function(v) paste0(v, "%")))),
      div(class = "grid2", if (length(m$rocs) && !is.null(m$importance)) div(class = "card", h3("What drives the prediction"), p(class = "small muted", "Share of importance from the random forest."), hbars(utils::head(m$importance$feature, 10), utils::head(m$importance$share, 10), fmt = function(v) paste0(v, "%"))),
          if (!is.null(cm_sel)) div(class = "card", h3("Where the model gets it wrong"), cm_sel)),
      rawcard, div(class = "row-btns", actionButton("next_step", "Continue to insights", class = "btn primary"), tags$button(type = "button", class = "btn", onclick = js_set("nav", "predict"), "Use this model on new data")))
  }
  output$cm_view <- renderUI({ req(rv$mod, input$cm_model); cm <- rv$mod$cms[[input$cm_model]]; req(cm); cm_ui(cm) })

  # ---------- Step 6: insights ----------
  observeEvent(input$run_insights, {
    req(rv$ml)
    withProgress(message = "Looking for structure in the data", value = .3, safe({
      X <- insight_matrix(rv$clean$data, rv$target, rv$ml); if (is.null(X)) stop("Not enough varying numeric features for insights.", call. = FALSE)
      incProgress(.2, detail = "Correlations and PCA"); co <- corr_pairs(rv$clean$data); pc <- pca_info(X)
      incProgress(.2, detail = "Clustering"); cl <- cluster_rows(X, seed = rv$ml$options$seed)
      incProgress(.2, detail = "Anomalies"); an <- find_anomalies(X)
      rv$insights <- list(corr = co, pca = pc, clusters = cl, anomalies = an, n = nrow(X))
      audit(rv$user$id, "insights", rv$name)
    }))
  })
  stage_insights <- function() {
    if (is.null(rv$ml)) return(div(class = "card empty", h3("Prepare your data first"), p("Finish step 4 to unlock insights.")))
    form <- div(class = "card", h3("6. Insights"), p(class = "muted", "Unsupervised machine learning finds structure you did not ask for: correlated features, natural groups of rows and unusual records."),
                actionButton("run_insights", if (is.null(rv$insights)) "Generate insights" else "Refresh", class = "btn primary"))
    if (is.null(rv$insights)) return(form)
    i <- rv$insights; pc <- i$pca; cl <- i$clusters; an <- i$anomalies
    flag <- if (!is.null(an)) an$all > an$cutoff else NULL
    tagList(form,
      if (!is.null(i$corr)) div(class = "grid2", div(class = "card", h3("Correlation map"), p(class = "small muted", "Green means two columns rise together, red means one rises as the other falls."), heatmap_ui(i$corr$matrix)),
          div(class = "card", h3("Strongest relationships"), tbl_ui(setNames(i$corr$pairs, c("Column A", "Column B", "Correlation")), 12), p(class = "small muted", style = "margin-top:10px", "Pairs above 0.9 are nearly duplicates. One of them can usually be dropped without losing information."))),
      div(class = "grid2", div(class = "card", h3("Natural groups (k-means)"), p(sprintf("Rinse found %d groups of rows (silhouette %.2f; above 0.5 is clear structure, below 0.25 is weak).", cl$k, cl$silhouette)),
            svg_scatter(pc$scores[, 1], pc$scores[, 2], group = cl$labels, flag = flag), div(class = "legend", lapply(seq_len(cl$k), function(g) span(tags$i(style = paste0("background:", PALETTE[(g - 1) %% length(PALETTE) + 1])), sprintf("Group %d (%d rows)", g, cl$size[g]))), if (!is.null(flag) && any(flag)) span(tags$i(style = "background:transparent;border:2px solid #DC2626;border-radius:50%"), "Unusual rows"))),
          div(class = "card", h3("What makes the groups different"), p(class = "small muted", "Average (scaled) value of the five most distinguishing features per group."), tbl_ui(cbind(Group = seq_len(cl$k), cl$centers), 10),
              h4(style = "margin-top:16px", "Dimensionality"), p(sprintf("The first two components capture %.0f%% of the variation. %d components are enough for 90%%.", 100 * pc$cum[min(2, length(pc$cum))], pc$n90)),
              hbars(paste0("PC", seq_len(min(6, length(pc$var)))), 100 * pc$var[seq_len(min(6, length(pc$var)))], vmax = 100, fmt = function(v) sprintf("%.0f%%", v)))),
      if (!is.null(an)) div(class = "card", h3("Unusual records"), p(sprintf("%d of %d rows (%.1f%%) are statistically far from the rest in several features at once (Mahalanobis distance above %.1f). They are not always errors, but they deserve a look.", an$n_flagged, i$n, an$pct, an$cutoff)),
          tbl_ui(setNames(an$top, c("Row (in sample)", "Distance")), 15)),
      div(class = "row-btns", actionButton("next_step", "Continue to export", class = "btn primary")))
  }

  # ---------- Predict ----------
  active_bundle <- reactive(rv$ext_bundle %||% rv$bundle)
  observeEvent(input$bundle_file, safe({
    b <- readRDS(input$bundle_file$datapath)
    if (!is.list(b) || !isTRUE(as.character(b$app)[1] %in% c(APP_NAME, "Sift")) || is.null(b$fit_obj) || is.null(b$clean_fit)) stop("That file is not a Rinse model.", call. = FALSE)
    rv$ext_bundle <- b; rv$pred <- NULL; showNotification("Model loaded.")
  }))
  observeEvent(input$use_own, { rv$ext_bundle <- NULL; rv$pred <- NULL })
  observeEvent(input$pred_model, { if (!is.null(rv$mod) && is.null(rv$ext_bundle) && input$pred_model %in% names(rv$mod$fits)) { rv$bundle <- make_bundle(rv$clean, rv$ml, rv$mod, input$pred_model); rv$pred <- NULL } })
  observeEvent(input$run_pred, {
    b <- active_bundle(); req(b, input$pred_file)
    withProgress(message = "Scoring rows", value = .5, safe({
      new <- read_any(input$pred_file$datapath, input$pred_file$name)
      miss <- setdiff(b$raw_columns, names(new)); if (length(miss) > 0 && length(miss) == length(b$raw_columns)) stop("None of the model's columns were found in that file.", call. = FALSE)
      rv$pred <- list(out = predict_new(b, new), missing = miss); audit(rv$user$id, "predict", sprintf("%d rows", nrow(new)))
    }))
  })
  observeEvent(input$run_one, {
    b <- active_bundle(); req(b); cols <- utils::head(b$raw_columns, 15)
    row <- as.data.frame(stats::setNames(lapply(seq_along(cols), function(k) input[[paste0("one_", k)]] %||% ""), cols), stringsAsFactors = FALSE, check.names = FALSE)
    for (c in setdiff(b$raw_columns, cols)) row[[c]] <- NA_character_
    safe(rv$one <- predict_new(b, row))
  })
  output$pred_dl <- downloadHandler(filename = function() "rinse_predictions.csv", content = function(f) { req(rv$pred); utils::write.csv(rv$pred$out, f, row.names = FALSE) })

  page_predict <- function() {
    b <- active_bundle()
    head <- div(class = "page-head", div(h1("Predict on new data"), p(class = "muted", "Score new rows with a trained model. Rinse cleans them with the same rules first.")))
    if (is.null(b)) return(tagList(head, div(class = "card empty", h3("No model yet"), p("Train models in the Analyze workflow (step 5), or load a saved Rinse model file below."),
      div(class = "row-btns", style = "justify-content:center", tags$button(type = "button", class = "btn primary", onclick = js_set("nav", "analyze"), "Go to Analyze")), div(style = "max-width:360px;margin:18px auto 0;text-align:left", fileInput("bundle_file", "Load a saved model (.rds)", accept = ".rds")))))
    cols <- utils::head(b$raw_columns, 15); ex <- if (!is.null(rv$raw) && is.null(rv$ext_bundle)) as.character(unlist(rv$raw[1, cols, drop = TRUE])) else rep("", length(cols))
    met <- b$metrics[1, ]
    tagList(head,
      div(class = "card", h3("Model"), div(class = "grid3", stat_box("Algorithm", b$model_name), stat_box(met$main_metric, sprintf("%.3f", met$main)), stat_box("Predicts", b$target)),
          if (!is.null(rv$mod) && is.null(rv$ext_bundle)) div(style = "margin-top:14px", selectInput("pred_model", "Use a different trained model", names(rv$mod$fits), b$model_name)),
          if (!is.null(rv$ext_bundle)) div(class = "row-btns", span(class = "chip warn", "Loaded from file"), actionButton("use_own", "Use this session's model instead", class = "btn sm ghost"))
          else div(style = "max-width:360px", fileInput("bundle_file", "Or load a saved model (.rds)", accept = ".rds"))),
      div(class = "card", h3("Score a file"), p(class = "muted", "Upload a CSV or Excel file with the same columns as the training data (the target column is not needed)."),
          p(class = "small muted", "Expected columns: ", paste(b$raw_columns, collapse = ", ")), fileInput("pred_file", NULL, buttonLabel = "Choose file", placeholder = "CSV or Excel", accept = c(".csv", ".xlsx", ".xls", ".tsv")),
          actionButton("run_pred", "Predict", class = "btn primary"),
          if (!is.null(rv$pred)) tagList(div(style = "height:16px"), if (length(rv$pred$missing)) div(class = "msg info", paste("Columns not found, filled with typical values:", paste(rv$pred$missing, collapse = ", "))),
            div(class = "grid3", stat_box("Rows scored", nrow(rv$pred$out)), if (b$task == "classification") stat_box("Most common prediction", names(sort(table(rv$pred$out$prediction), decreasing = TRUE))[1]) else stat_box("Average prediction", round(mean(rv$pred$out$prediction), 2)),
                 if (b$task == "classification") stat_box("Average confidence", paste0(round(100 * mean(rv$pred$out$confidence)), "%")) else stat_box("Range", paste(round(range(rv$pred$out$prediction), 1), collapse = " to "))),
            div(style = "height:12px"), DT::DTOutput("tbl_pred"), div(class = "row-btns", downloadButton("pred_dl", "Download predictions (CSV)", class = "btn primary")))),
      div(class = "card", h3("Score one record"), p(class = "muted", "Type values for one row. Leave a box empty if unknown."),
          div(class = "grid3", lapply(seq_along(cols), function(k) textInput(paste0("one_", k), cols[k], value = ex[k]))),
          if (length(b$raw_columns) > 15) p(class = "small muted", sprintf("Showing 15 of %d columns; the rest use typical values.", length(b$raw_columns))),
          actionButton("run_one", "Predict this record", class = "btn primary"),
          if (!is.null(rv$one)) div(style = "margin-top:14px", span(class = "chip ok", "Prediction"), tags$h2(style = "margin:8px 0 2px", as.character(rv$one$prediction[1])),
                                    if (!is.null(rv$one$confidence)) p(class = "muted", sprintf("Confidence %.0f%%", 100 * rv$one$confidence[1])))))
  }
  output$tbl_pred <- DT::renderDT({ req(rv$pred); dt_table(rv$pred$out, page = 10, filter = FALSE, buttons = FALSE) })

  # ---------- Drift ----------
  observeEvent(input$run_drift, {
    req(input$new_file)
    withProgress(message = "Comparing datasets", value = .5, safe({
      ref <- if (identical(input$ref_src, "current") && !is.null(rv$raw)) rv$raw else { req(input$ref_file); read_any(input$ref_file$datapath, input$ref_file$name) }
      new <- read_any(input$new_file$datapath, input$new_file$name)
      rv$drift <- list(rep = drift_report(ref, new), ref_n = nrow(ref), new_n = nrow(new), ref_cols = setdiff(names(ref), names(new)), new_cols = setdiff(names(new), names(ref)))
      audit(rv$user$id, "drift_check", sprintf("%d vs %d rows", nrow(ref), nrow(new)))
    }))
  })
  page_drift <- function() {
    d <- rv$drift
    tagList(div(class = "page-head", div(h1("Drift check"), p(class = "muted", "Has new data moved away from the data your model learned from? Drift is a common reason models quietly get worse."))),
      div(class = "card", h3("Compare two datasets"),
          radioButtons("ref_src", "Reference (the data you trained on)", c(if (!is.null(rv$raw)) c(`Current dataset in Analyze` = "current"), `Upload a file` = "file"), selected = if (!is.null(rv$raw)) "current" else "file"),
          conditionalPanel("input.ref_src == 'file'", fileInput("ref_file", "Reference file", accept = c(".csv", ".xlsx", ".xls", ".tsv", ".json"))),
          fileInput("new_file", "New data to check", accept = c(".csv", ".xlsx", ".xls", ".tsv", ".json")), actionButton("run_drift", "Check for drift", class = "btn primary")),
      if (!is.null(d)) { r <- d$rep; sig <- sum(r$status == "Significant drift"); mod <- sum(r$status == "Moderate drift")
        tagList(div(class = "card", h3("Result"), div(class = "grid4 keep2", stat_box("Columns compared", nrow(r)), stat_box("Significant drift", sig, if (sig) "bad" else "good"), stat_box("Moderate drift", mod), stat_box("Rows (reference / new)", paste(format(d$ref_n, big.mark = ","), "/", format(d$new_n, big.mark = ",")))),
            p(style = "margin-top:12px", if (sig) sprintf("%d column(s) look very different from the reference. A model trained on the reference data may be unreliable on this data.", sig) else if (mod) "Some columns have shifted a little. Keep watching them." else "The new data looks like the reference data."),
            if (length(d$ref_cols) || length(d$new_cols)) div(class = "msg info", paste0(if (length(d$ref_cols)) paste("Missing from new data:", paste(d$ref_cols, collapse = ", "), ". "), if (length(d$new_cols)) paste("New columns:", paste(d$new_cols, collapse = ", "), ".")))),
          div(class = "card", h3("Shift by column (PSI)"), p(class = "small muted", "Population Stability Index: under 0.1 is stable, 0.1 to 0.25 is a moderate shift, above 0.25 is a significant shift."),
              hbars(r$column, r$psi, vmax = max(0.5, max(r$psi)), fmt = function(v) sprintf("%.2f", v), tone = "lost"), div(style = "height:12px"), DT::DTOutput("tbl_drift"))) })
  }
  output$tbl_drift <- DT::renderDT({ req(rv$drift); r <- rv$drift$rep; dt_table(data.frame(Column = r$column, Type = r$type, PSI = r$psi, Status = r$status, `Reference mean` = r$ref_mean, `New mean` = r$new_mean, `New missing %` = r$new_missing_pct, check.names = FALSE), filter = FALSE) })

  list(stage_models = stage_models, stage_insights = stage_insights, page_predict = page_predict, page_drift = page_drift)
}
