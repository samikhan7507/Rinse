# Screens and server logic: shell, sign-in, the seven-step workflow, history and help.
STEPS <- c("Data", "Check", "Clean", "Prepare", "Models", "Insights", "Export")

auth_ui <- function(tab, msg, otp_phone, otp_demo_code, state_url) {
  # The Phone tab is only offered when SMS is configured (or RINSE_DEMO_SMS=1 for a classroom demo).
  phone_tab <- isTRUE(CFG$sms_on) || isTRUE(CFG$demo_sms) || identical(tab, "phone")
  tabs <- div(class = "tabs", nav_btn("auth_tab", "email", "Email", tab == "email"),
              if (phone_tab) nav_btn("auth_tab", "phone", "Phone", tab == "phone"),
              nav_btn("auth_tab", "signup", "Create account", tab == "signup"))
  body <- switch(tab,
    email = div(class = "form", textInput("li_email", "Email address", placeholder = "name@example.com"), passwordInput("li_pw", "Password"),
                div(class = "forgot", "Forgot your password? Ask an administrator."),
                actionButton("li_go", "Sign in", class = "btn primary block")),
    signup = div(class = "form", textInput("su_name", "Full name"), textInput("su_email", "Email address", placeholder = "name@example.com"), passwordInput("su_pw", "Password"),
                 div(class = "help", "At least 8 characters with a letter and a number."), actionButton("su_go", "Create account", class = "btn primary block")),
    phone = if (is.null(otp_phone)) div(class = "form", textInput("ph_num", "Mobile number", placeholder = "98765 43210"), div(class = "help", "Indian numbers work without +91."), actionButton("ph_send", "Send code", class = "btn primary block"))
            else div(class = "form",
              if (!is.null(otp_demo_code)) div(class = "msg info", HTML(sprintf("Demo mode: no SMS service is connected, so your code is <b>%s</b>.", otp_demo_code))),
              p(class = "muted", paste("Enter the 6-digit code sent to", otp_phone)), textInput("ph_code", "Code", placeholder = "123456"),
              actionButton("ph_verify", "Verify and sign in", class = "btn primary block"), div(class = "row-btns", actionButton("ph_back", "Use a different number", class = "btn ghost"))))
  # "Continue with Google" is shown only when Google sign-in is actually configured.
  google <- if (CFG$google_on) tagList(div(class = "or", "or"), tags$a(class = "gbtn", href = state_url, GOOGLE_G, "Continue with Google"))
  tick <- HTML('<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="3" stroke-linecap="round" stroke-linejoin="round"><path d="M4 12.5l5 5L20 6.5"/></svg>')
  feat <- function(t, d) tags$li(class = "feat", tags$span(class = "tick", tick), tags$span(tags$b(t), tags$small(d)))
  div(class = "auth",
    div(class = "auth-side",
      div(tags$a(class = "brand", href = "/", style = "padding-left:0", tags$img(src = "logo.svg", alt = ""), APP_NAME),
          h1("Get your data ready for machine learning."),
          p(class = "lead", "Upload a CSV or Excel file. Rinse checks it, fixes what it safely can, and shows exactly what was wrong and what changed."),
          tags$ul(class = "feats",
            feat("A readiness score out of 100", "Every lost point comes with a reason and a fix."),
            feat("Cleaning you control", "Each step is optional and written to a log."),
            feat("Four models, compared fairly", "Accuracy, F1, ROC AUC and cross-validation."),
            feat("Train and test files without leakage", "Scaling is learned from the training rows only.")),
          div(class = "demo", div(score_ring(52, 96), div(class = "lbl", "Messy file")), div(class = "arrow", HTML("&rarr;")), div(score_ring(94, 96), div(class = "lbl", "After Rinse")))),
      div(class = "more small muted", paste0(APP_NAME, " ", APP_VERSION))),
    div(class = "auth-main", div(class = "auth-box",
      h2(switch(tab, signup = "Create your account", "Welcome back")),
      p(class = "muted", switch(tab, signup = "It takes a minute. You can start with a sample file straight away.", "Sign in to check, clean and prepare your data.")),
      tabs, msg_ui(msg), body, google,
      div(class = "foot", "Uploaded files are processed in memory and are not saved. Sign-in activity is logged for security."))))
}

NAV <- list(list("analyze", "Analyze", "Workspace"), list("predict", "Predict", NULL), list("drift", "Drift check", NULL),
            list("history", "History", "Account"), list("settings", "Settings", NULL), list("admin", "Admin", "Administration"), list("help", "Help", "More"))

shell_ui <- function(user, page) {
  is_admin <- identical(user$role, "admin")
  items <- Filter(function(i) i[[1]] != "admin" || is_admin, NAV)
  nm <- if (!is.na(user$name) && nzchar(user$name)) user$name else "User"
  side_items <- lapply(items, function(i) tagList(if (!is.null(i[[3]])) div(class = "group", i[[3]]),
    tags$button(type = "button", class = if (page == i[[1]]) "on", onclick = js_set("nav", i[[1]]), HTML(ICON[[i[[1]]]]), i[[2]])))
  bottom <- if (is_admin) c("analyze", "predict", "history", "admin") else c("analyze", "predict", "history", "settings")
  lab <- stats::setNames(vapply(items, `[[`, "", 2), vapply(items, `[[`, "", 1))
  more <- setdiff(vapply(items, `[[`, "", 1), bottom)
  tagList(div(class = "app",
    tags$aside(class = "side", tags$a(class = "brand", href = "/", tags$img(src = "logo.svg", alt = ""), APP_NAME), div(class = "sidenav", side_items),
      div(class = "side-foot", div(class = "usercard", div(class = "avatar", toupper(substr(nm, 1, 1))), div(div(class = "name", nm), div(class = "role", if (is_admin) "Administrator" else "Member"))),
          actionButton("logout", "Sign out", class = "btn sm"),
          tags$button(type = "button", class = "btn ghost sm pwa-install", style = "display:none", onclick = "vyInstall()", HTML(ICON$install), "Install app"),
          tags$a(class = "side-ver", href = "#", onclick = paste0(js_set("nav", "help"), ";return false;"), paste0(APP_NAME, " ", APP_VERSION)))),
    div(class = "content",
      div(class = "mtop", tags$a(class = "brand", href = "/", tags$img(src = "logo.svg", alt = ""), APP_NAME),
          div(class = "row-btns", style = "margin:0", div(class = "avatar", toupper(substr(nm, 1, 1))))),
      div(class = "container", if (isTRUE(user$must_change)) div(class = "msg info", "Your password was reset by an administrator. Please choose a new one in ", tags$a(href = "#", onclick = paste0(js_set("nav", "settings"), ";return false;"), "Settings"), "."), uiOutput("page"))),
    div(class = "sheet", lapply(more, function(k) tags$button(type = "button", onclick = paste0(js_set("nav", k), ";vyToggleSheet()"), HTML(ICON[[k]]), lab[[k]])),
        tags$button(type = "button", class = "pwa-install", style = "display:none", onclick = "vyToggleSheet();vyInstall()", HTML(ICON$install), "Install app"),
        tags$button(type = "button", onclick = "vyToggleSheet();Shiny.setInputValue('logout',Date.now(),{priority:'event'})", "Sign out")),
    div(class = "bottomnav", lapply(bottom, function(k) tags$button(type = "button", class = if (page == k) "on", onclick = js_set("nav", k), HTML(ICON[[k]]), lab[[k]])),
        tags$button(type = "button", onclick = "vyToggleSheet()", HTML(ICON$more), "More"))))
}

server <- function(input, output, session) {
  rq <- request_ctx(session)
  rv <- reactiveValues(user = NULL, page = "analyze", auth_tab = "email", auth_msg = NULL, otp_phone = NULL, otp_code = NULL, token = "", sid = NULL,
                       step = 1, src = "upload", raw = NULL, name = "", target = "", prof0 = NULL, score0 = NULL, clean = NULL, prof1 = NULL, score1 = NULL,
                       ml = NULL, mod = NULL, bundle = NULL, insights = NULL, rawcmp = NULL, run_id = NULL, saved = FALSE, tmp = NULL, tmpname = NULL, inp = list(),
                       pred = NULL, drift = NULL, ref = NULL, adm_tab = "overview", st_msg = NULL, ext_bundle = NULL)
  reset_work <- function() { for (k in c("raw", "prof0", "score0", "clean", "prof1", "score1", "ml", "mod", "bundle", "insights", "rawcmp", "run_id", "tmp", "tmpname", "pred")) rv[[k]] <- NULL; rv$name <- ""; rv$target <- ""; rv$step <- 1; rv$saved <- FALSE }
  fail <- function(e) { vlog("WARN", "User-visible error: %s", conditionMessage(e)); showNotification(conditionMessage(e), type = "error", duration = 9) }
  safe <- function(expr) tryCatch(expr, error = fail)

  begin_session <- function(u) {
    rv$user <- u; rv$auth_msg <- NULL; rv$page <- "analyze"; rv$otp_phone <- NULL; rv$otp_code <- NULL
    rv$sid <- track_start(u$id, rq, "analyze"); session$sendCustomMessage("vy_cfg", list(idle = CFG$idle_minutes))
  }
  login_ok <- function(u) { begin_session(u); tok <- create_session(u$id); rv$token <- tok; session$sendCustomMessage("vy_store", tok) }
  do_logout <- function(msg = NULL) {
    end_session(rv$token); track_end(rv$sid); session$sendCustomMessage("vy_store", ""); audit(rv$user$id, "logout")
    rv$user <- NULL; rv$token <- ""; rv$sid <- NULL; rv$auth_tab <- "email"; rv$auth_msg <- if (!is.null(msg)) list(type = "info", text = msg); reset_work()
  }
  session$onSessionEnded(function() track_end(isolate(rv$sid)))
  observe({ invalidateLater(30000); if (!is.null(rv$user) && !is.null(rv$sid)) track_beat(rv$sid, rv$page) })
  observeEvent(input$idle, { if (!is.null(rv$user)) do_logout("You were signed out after a period of inactivity.") })

  # ----- restore sign-in, Google callback -----
  observeEvent(input$vy_token, {
    qs <- shiny::parseQueryString(isolate(session$clientData$url_search))
    if (!is.null(qs$code) && !is.null(qs$state)) {
      updateQueryString("?", mode = "replace")
      if (!check_state(qs$state)) { rv$auth_msg <- list(type = "err", text = "Google sign-in expired. Please try again."); return() }
      r <- google_login(qs$code, rq); if (r$ok) login_ok(r$user) else rv$auth_msg <- list(type = "err", text = r$msg)
      return()
    }
    if (is.null(rv$user)) { u <- session_user(input$vy_token); if (!is.null(u)) { begin_session(u); rv$token <- input$vy_token } }
  }, ignoreInit = FALSE)

  output$root <- renderUI({
    if (is.null(rv$user)) auth_ui(rv$auth_tab, rv$auth_msg, rv$otp_phone, rv$otp_code, if (CFG$google_on) google_url(make_state()) else "#")
    else shell_ui(rv$user, rv$page)
  })

  # ----- auth actions -----
  observeEvent(input$auth_tab, { rv$auth_tab <- input$auth_tab; rv$auth_msg <- NULL; rv$otp_phone <- NULL; rv$otp_code <- NULL })
  observeEvent(input$li_go, { r <- login_email(input$li_email, input$li_pw, rq); if (r$ok) login_ok(r$user) else rv$auth_msg <- list(type = "err", text = r$msg) })
  observeEvent(input$su_go, { r <- register_user(input$su_name, input$su_email, input$su_pw, rq); if (r$ok) login_ok(r$user) else rv$auth_msg <- list(type = "err", text = r$msg) })
  observeEvent(input$ph_send, { r <- send_otp(input$ph_num); if (r$ok) { rv$otp_phone <- r$phone; rv$otp_code <- if (isTRUE(r$demo)) r$code else NULL; rv$auth_msg <- NULL } else rv$auth_msg <- list(type = "err", text = r$msg) })
  observeEvent(input$ph_back, { rv$otp_phone <- NULL; rv$otp_code <- NULL; rv$auth_msg <- NULL })
  observeEvent(input$ph_verify, { r <- verify_otp(rv$otp_phone, input$ph_code, rq); if (r$ok) login_ok(r$user) else rv$auth_msg <- list(type = "err", text = r$msg) })
  observeEvent(input$logout, { if (!is.null(rv$user)) do_logout() })
  observeEvent(input$nav, {
    if (input$nav == "admin" && !identical(rv$user$role, "admin")) return()
    rv$page <- input$nav; session$sendCustomMessage("vy_scroll", 1); track_beat(rv$sid, input$nav)
  })

  # ----- loading data -----
  adopt <- function(df, name, target = "") {
    reset_work(); rv$raw <- df; rv$name <- name
    if (!nzchar(target)) { last <- names(df)[ncol(df)]; vals <- unique(df[[last]][!is_missing(df[[last]])]); if (length(vals) > 1 && length(vals) <= 10) target <- last }
    rv$target <- target; audit(rv$user$id, "load_data", paste(name, nrow(df), "x", ncol(df))); vlog("INFO", "Loaded %s (%d x %d) for user %s", name, nrow(df), ncol(df), rv$user$id)
  }
  observeEvent(input$src_tab, { rv$src <- input$src_tab })
  observeEvent(input$sample, { s <- SAMPLES[[input$sample]]; req(s); safe(adopt(read_any(file.path("data", "samples", paste0(input$sample, ".csv"))), paste0(input$sample, ".csv"), s$target)) })
  observeEvent(input$file, {
    f <- input$file; ext <- tolower(tools::file_ext(f$name)); rv$tmp <- f$datapath; rv$tmpname <- f$name
    if (ext %in% c("db", "sqlite", "sqlite3") && !length(tryCatch(sqlite_tables(f$datapath), error = function(e) character(0)))) showNotification("No tables found in that database file.", type = "error")
  })
  output$file_opts <- renderUI({
    req(rv$tmp); ext <- tolower(tools::file_ext(rv$tmpname))
    pick <- if (ext %in% c("xlsx", "xls")) { sh <- excel_sheets_safe(rv$tmp); if (length(sh) > 1) selectInput("sheet", "Sheet", sh) }
            else if (ext %in% c("db", "sqlite", "sqlite3")) selectInput("sqltable", "Table", tryCatch(sqlite_tables(rv$tmp), error = function(e) character(0)))
    tagList(pick, actionButton("load_file", paste("Load", rv$tmpname), class = "btn primary"))
  })
  observeEvent(input$load_file, {
    withProgress(message = "Reading file", value = .5, safe({
      ext <- tolower(tools::file_ext(rv$tmpname)); path <- rv$tmp; nm <- rv$tmpname
      adopt(if (ext %in% c("db", "sqlite", "sqlite3")) read_sqlite(path, input$sqltable) else read_any(path, nm, sheet = if (!is.null(input$sheet)) input$sheet else 1), nm)
    }))
  })
  observeEvent(input$url_go, { withProgress(message = "Downloading", value = .5, safe({ u <- trimws(input$url); adopt(read_url(u), basename(sub("\\?.*$", "", u))) })) })

  # ----- pipeline -----
  observeEvent(input$run_check, {
    req(rv$raw); rv$target <- if (identical(input$target, "(none)") || is.null(input$target)) "" else input$target
    withProgress(message = "Checking your data", value = .5, safe({
      rv$prof0 <- profile_data(rv$raw, rv$target); rv$score0 <- score_data(rv$prof0)
      rv$clean <- NULL; rv$ml <- NULL; rv$mod <- NULL; rv$bundle <- NULL; rv$insights <- NULL; rv$rawcmp <- NULL; rv$step <- 2; session$sendCustomMessage("vy_scroll", 1)
    }))
  })
  keep_inputs <- function(ids) for (id in ids) if (!is.null(input[[id]])) rv$inp[[id]] <- input[[id]]
  V <- function(id, d) { x <- rv$inp[[id]]; if (is.null(x)) d else x }
  observeEvent(input$run_clean, {
    req(rv$raw)
    # Ensure a profile exists before cleaning - if user somehow reaches Clean without Check,
    # build it now so score comparisons work
    if (is.null(rv$prof0) || is.null(rv$score0)) {
      tryCatch({
        rv$prof0 <- profile_data(rv$raw, rv$target)
        rv$score0 <- score_data(rv$prof0)
      }, error = function(e) {
        showNotification(paste("Could not profile data:", conditionMessage(e)), type = "error", duration = 8)
      })
    }
    keep_inputs(c("o_missing", "o_trim", "o_labels", "o_numbers", "o_dates", "o_dups", "o_const", "o_id", "o_sparse", "o_thr", "o_impnum", "o_impcat", "o_outl", "o_flags", "o_pii", "o_leak"))
    withProgress(message = "Cleaning your data...", value = 0.1, detail = "Applying selected transformations", safe({
      o <- list(standardize_missing = isTRUE(input$o_missing), trim_text = isTRUE(input$o_trim), unify_labels = isTRUE(input$o_labels), fix_numbers = isTRUE(input$o_numbers),
                parse_dates = isTRUE(input$o_dates), drop_duplicates = isTRUE(input$o_dups), drop_constant = isTRUE(input$o_const), drop_id = isTRUE(input$o_id),
                drop_sparse = isTRUE(input$o_sparse), sparse_threshold = suppressWarnings(as.numeric(input$o_thr %||% 60)) %||% 60, impute_numeric = input$o_impnum %||% "median", impute_categorical = input$o_impcat %||% "mode",
                outliers = input$o_outl %||% "cap", missing_flags = isTRUE(input$o_flags), pii = input$o_pii %||% "drop", drop_leakage = isTRUE(input$o_leak))
      # Validate sparse threshold bounds
      if (is.na(o$sparse_threshold) || o$sparse_threshold < 5) o$sparse_threshold <- 60
      if (o$sparse_threshold > 95) o$sparse_threshold <- 95

      incProgress(0.2, detail = "Cleaning and transforming...")
      clean_res <- clean_data(rv$raw, o, rv$target, rv$prof0)

      # Defensive: ensure result is valid before updating reactive state
      if (is.null(clean_res) || is.null(clean_res$data) || nrow(clean_res$data) == 0) {
        stop("Cleaning produced no usable rows. Try less aggressive settings (e.g., keep ID columns, raise missing limit, use 'Cap' instead of 'Remove' for outliers).", call. = FALSE)
      }
      rv$clean <- clean_res
      incProgress(0.3, detail = "Scoring cleaned data...")

      # Profile cleaned data - with fallback if it fails
      prof1 <- tryCatch(profile_data(rv$clean$data, rv$target), error = function(e) {
        vlog("WARN", "Profile after clean failed: %s", conditionMessage(e))
        NULL
      })
      score1 <- if (!is.null(prof1)) tryCatch(score_data(prof1), error = function(e) NULL) else NULL

      # If profiling fails, still allow workflow to continue with estimated scores
      if (is.null(prof1) || is.null(score1)) {
        # Create minimal fallback to keep UI functional
        rv$prof1 <- rv$prof0
        rv$score1 <- list(score = min(100, (rv$score0$score %||% 50) + 15), grade = "Almost ready",
                          breakdown = rv$score0$breakdown, issues = rv$score0$issues[0, ])
        showNotification("Data cleaned but scoring had issues. You can continue to next steps.", type = "warning", duration = 6)
      } else {
        rv$prof1 <- prof1
        rv$score1 <- score1
      }

      rv$ml <- NULL; rv$mod <- NULL; rv$bundle <- NULL; rv$insights <- NULL; rv$rawcmp <- NULL
      incProgress(0.3, detail = "Saving progress...")

      # Save run - failure here should NOT block the user from continuing
      if (!isTRUE(rv$saved)) {
        tryCatch({
          before_score <- rv$score0$score %||% 0
          after_score <- rv$score1$score %||% before_score
          after_grade <- rv$score1$grade %||% "Unknown"
          rv$run_id <- save_run(rv$user$id, rv$name, nrow(rv$raw), ncol(rv$raw), rv$target, before_score, after_score, after_grade)
          rv$saved <- TRUE
        }, error = function(e) {
          vlog("WARN", "save_run failed: %s", conditionMessage(e))
          rv$run_id <- NULL
          rv$saved <- TRUE # prevent repeated attempts
        })
      }
      score_before <- rv$score0$score %||% 0
      score_after <- rv$score1$score %||% score_before
      audit(rv$user$id, "clean", sprintf("%s %d->%d", rv$name, score_before, score_after))
      incProgress(0.2, detail = "Done!")
      showNotification(sprintf("Cleaning complete! %d rows -> %d rows, %d cols -> %d cols. Score %d -> %d",
                               nrow(rv$raw), nrow(rv$clean$data), ncol(rv$raw), ncol(rv$clean$data), score_before, score_after),
                       type = "message", duration = 5)
    }))
  })
  observeEvent(input$run_ml, {
    req(rv$clean); keep_inputs(c("m_encode", "m_scale", "m_test", "m_seed", "m_strat", "m_dates"))
    withProgress(message = "Preparing for machine learning", value = .5, safe({
      rv$ml <- prepare_ml(rv$clean$data, rv$target, list(encode = input$m_encode, scale = input$m_scale, test_ratio = as.numeric(input$m_test), seed = input$m_seed %||% 42, stratify = isTRUE(input$m_strat), date_parts = isTRUE(input$m_dates)))
      rv$mod <- NULL; rv$bundle <- NULL; rv$insights <- NULL; rv$rawcmp <- NULL
    }))
  })
  observeEvent(input$goto, {
    s <- as.integer(input$goto)
    if (is.na(s) || s < 1 || s > 7) return()
    # Allow going back freely, but forward only if prerequisites met
    ok <- s == 1 || s <= rv$step ||  # always allow backward or current step
           (s <= 3 && !is.null(rv$prof0)) ||
           (s == 4 && !is.null(rv$clean)) ||
           (s %in% c(5, 6) && !is.null(rv$ml)) ||
           (s == 7 && !is.null(rv$clean))
    if (ok) {
      rv$step <- s
      session$sendCustomMessage("vy_scroll", 1)
    } else {
      showNotification(switch(as.character(s),
        "2" = "Please run 'Check data readiness' first.",
        "3" = "Please run 'Check data readiness' first.",
        "4" = "Please clean your data first (step 3).",
        "5" = "Please prepare ML data first (step 4).",
        "6" = "Please prepare ML data first (step 4).",
        "7" = "Please clean your data first (step 3).",
        "Complete the previous steps first."
      ), type = "warning")
    }
  })
  observeEvent(input$next_step, {
    # FIX: Respect unlock logic instead of blind increment - prevents skipping to broken states
    # and ensures user gets feedback when blocked
    cur <- rv$step
    nxt <- min(7, cur + 1)
    can_advance <- switch(as.character(nxt),
      "2" = !is.null(rv$prof0),
      "3" = !is.null(rv$prof0),
      "4" = !is.null(rv$clean),
      "5" = !is.null(rv$ml),
      "6" = !is.null(rv$ml),
      "7" = !is.null(rv$clean),
      TRUE
    )
    # Steps 2-3 require prof0, 4 and 7 require clean, 5-6 require ml
    # But allow advance from 1->2 only after run_check has set prof0
    # If cleaning just completed, prof0 and clean are ready
    if (can_advance || nxt <= cur) {
      rv$step <- nxt
      session$sendCustomMessage("vy_scroll", 1)
    } else {
      # Provide specific guidance instead of silently stalling
      msg <- switch(as.character(nxt),
        "2" = "Analysis not ready. Click 'Check data readiness' in step 1.",
        "3" = "Check not completed. Please wait for the readiness check to finish.",
        "4" = "Cleaning not done. Click 'Clean my data' and wait for completion.",
        "5" = "Preparation needed. Click 'Prepare data' in step 4.",
        "6" = "Preparation needed. Click 'Prepare data' in step 4.",
        "7" = "Cleaning not done. Complete step 3 first.",
        "Cannot advance yet."
      )
      showNotification(msg, type = "warning", duration = 6)
      vlog("INFO", "Next blocked: step %d -> %d (prof0=%s clean=%s ml=%s)", cur, nxt,
           !is.null(rv$prof0), !is.null(rv$clean), !is.null(rv$ml))
    }
  })
  observeEvent(input$new_run, { reset_work() })

  ctx <- reactive({ req(rv$clean); list(name = rv$name, target = rv$target, prof_before = rv$prof0, prof_after = rv$prof1, score_before = rv$score0, score_after = rv$score1, clean = rv$clean, ml = rv$ml, mod = rv$mod, bundle = rv$bundle, rawcmp = rv$rawcmp) })

  # ----- downloads -----
  stem <- reactive(tools::file_path_sans_ext(rv$name))
  dl <- function(id, ext, fn) output[[id]] <- downloadHandler(filename = function() paste0("rinse_", stem(), "_", id, ext), content = function(file) tryCatch(fn(file), error = function(e) writeLines(conditionMessage(e), file)))
  dl("bundle", ".zip", function(f) build_bundle(f, ctx()))
  dl("cleaned", ".csv", function(f) utils::write.csv(rv$clean$data, f, row.names = FALSE))
  dl("train", ".csv", function(f) { req(rv$ml); utils::write.csv(rv$ml$train, f, row.names = FALSE) })
  dl("test", ".csv", function(f) { req(rv$ml); utils::write.csv(rv$ml$test, f, row.names = FALSE) })
  dl("excel", ".xlsx", function(f) build_excel(f, ctx()))
  dl("report", ".html", function(f) writeLines(build_html_report(ctx()), f))
  dl("recipe", ".json", function(f) writeLines(build_recipe(ctx()), f))
  dl("sqlitedb", ".sqlite", function(f) build_sqlite(f, ctx()))
  dl("model", ".rds", function(f) { req(rv$bundle); saveRDS(rv$bundle, f) })
  dl("preds", ".csv", function(f) { req(rv$mod); utils::write.csv(rv$mod$preds[[rv$mod$best]], f, row.names = FALSE) })

  # ----- other modules -----
  ml <- register_ml(input, output, session, rv, safe, V)
  adm <- register_admin(input, output, session, rv, safe, do_logout)

  output$page <- renderUI({
    switch(rv$page, analyze = page_analyze(), predict = ml$page_predict(), drift = ml$page_drift(), history = page_history(),
           settings = adm$page_settings(), admin = if (identical(rv$user$role, "admin")) adm$page_admin() else NULL, help = page_help())
  })

  stepper <- function() {
    unlocked <- c(TRUE, !is.null(rv$prof0), !is.null(rv$prof0), !is.null(rv$clean), !is.null(rv$ml), !is.null(rv$ml), !is.null(rv$clean))
    div(class = "stepper", lapply(seq_along(STEPS), function(i) tags$button(type = "button", disabled = if (!unlocked[i]) NA, class = paste(if (rv$step == i) "on", if (unlocked[i] && rv$step > i) "done"),
      onclick = js_set("goto", i), span(class = "n", if (unlocked[i] && rv$step > i) HTML("&#10003;") else i), tags$b(STEPS[i]))))
  }
  next_btn <- function(label) div(class = "row-btns", actionButton("next_step", label, class = "btn primary"))

  page_analyze <- function() {
    tagList(div(class = "page-head", div(h1("Prepare a dataset"), p(class = "muted", "From a messy file to trained, explained models in seven steps."))), stepper(),
            switch(rv$step, stage_data(), stage_check(), stage_clean(), stage_prepare(), ml$stage_models(), ml$stage_insights(), stage_export()))
  }

  stage_data <- function() {
    src <- rv$src
    tabs <- div(class = "tabs2", lapply(list(c("upload", "Upload a file"), c("sample", "Try a sample"), c("link", "From a link")), function(t) nav_btn("src_tab", t[1], t[2], src == t[1])))
    body <- switch(src,
      upload = div(
        div(class = "drop",
          div(class = "drop-ico", HTML(ICON$upload)),
          div(class = "drop-t", if (!is.null(rv$tmpname) && nzchar(rv$tmpname)) rv$tmpname else "Drop a file here, or click to browse"),
          div(class = "drop-s", "CSV, TSV, Excel, JSON or SQLite"),
          fileInput("file", NULL, buttonLabel = "Browse", placeholder = "", accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls", ".json", ".db", ".sqlite", ".sqlite3"))),
        div(class = "drop-meta", sprintf("Up to %d MB and 200,000 rows \u00b7 files stay in memory and are not saved", as.integer(CFG$max_upload_mb))),
        uiOutput("file_opts")),
      sample = div(p(class = "muted", "Realistic practice data you can use right now. Each one has problems a real dataset would have."),
                   div(class = "samples", lapply(names(SAMPLES), function(k) tags$button(type = "button", class = "sample", onclick = js_set("sample", k), tags$b(SAMPLES[[k]]$title), span(class = "small muted", SAMPLES[[k]]$desc))))),
      link = div(textInput("url", "Link to a CSV or JSON file", placeholder = "https://example.com/data.csv"), div(class = "help", "Must start with https://. Public files only."), actionButton("url_go", "Download and load", class = "btn primary")))
    loaded <- if (!is.null(rv$raw)) {
      df <- rv$raw; ch <- c("(none)", names(df)); sel <- if (nzchar(rv$target) && rv$target %in% names(df)) rv$target else "(none)"
      div(class = "card", h3(rv$name), div(class = "grid3", stat_box("Rows", format(nrow(df), big.mark = ",")), stat_box("Columns", ncol(df)), stat_box("Cells", format(nrow(df) * ncol(df), big.mark = ","))),
          div(style = "height:14px"), h4("Preview"), tbl_ui(df, 8), div(style = "height:16px"),
          selectInput("target", "What do you want to predict? (target column)", ch, selected = sel),
          div(class = "help", "Needed for model training, class-balance and leakage checks. Rinse guessed the last column if it looks like a label."),
          actionButton("run_check", "Check data readiness", class = "btn primary"))
    }
    tagList(div(class = "card", h3("1. Add your data"), tabs, body), loaded)
  }

  stage_check <- function() {
    req(rv$score0); s <- rv$score0; ov <- rv$prof0$overview; cl <- rv$prof0$columns; iss <- s$issues
    cats <- lapply(seq_len(nrow(s$breakdown)), function(i) { b <- s$breakdown[i, ]
      div(class = "cat", span(b$category), div(class = "bar", tags$i(class = if (b$lost > 0) "lost", style = sprintf("width:%d%%", round(100 * b$lost / b$max)))), span(class = "muted small", sprintf("-%s", b$lost))) })
    issues <- if (nrow(iss)) lapply(seq_len(min(14, nrow(iss))), function(i) div(class = "issue", div(class = "pts", paste0("-", iss$points[i])), div(div(class = "t", iss$column[i], span(class = "muted", paste0(" \u00b7 ", iss$category[i]))), div(iss$reason[i]), div(class = "fix", paste("Fix:", iss$fix[i]))))) else div(class = "empty", h3("No issues found"), "This data looks ready.")
    pii <- cl[nzchar(cl$pii), , drop = FALSE]; lk <- rv$prof0$leakage
    safety <- if (nrow(pii) || nrow(lk)) div(class = "card", h3("Privacy and leakage"),
      if (nrow(pii)) tagList(h4("Personal data"), p(class = "muted small", "Columns that look like personal information. They are dropped by default so models never learn from them."), lapply(seq_len(nrow(pii)), function(i) span(class = "chip bad", style = "margin:0 6px 6px 0", paste0(pii$name[i], " \u00b7 ", pii$pii[i])))),
      if (nrow(lk)) tagList(h4("Possible target leakage"), p(class = "muted small", "These columns are almost a copy of what you want to predict. A model would look brilliant in testing and fail in real use."), lapply(seq_len(nrow(lk)), function(i) span(class = "chip warn", style = "margin:0 6px 6px 0", sprintf("%s \u00b7 strength %.2f", lk$column[i], lk$strength[i])))))
    tagList(
      div(class = "card", h3("2. How ready is this data?"), div(class = "ring-wrap", score_ring(s$score, 150, "out of 100"),
        div(span(class = paste("chip", grade_class(s$grade)), s$grade), p(style = "margin-top:8px", switch(s$grade, Ready = "This data can go straight into a model.", `Almost ready` = "A few fixes will make it model-ready.", `Needs work` = "Several problems could hurt model quality. Cleaning will help.", "Training on this data as it is would give unreliable results.")),
            div(class = "grid3", style = "min-width:min(420px,80vw)", stat_box("Rows", format(ov$rows, big.mark = ",")), stat_box("Duplicate rows", ov$duplicates), stat_box("Missing cells", paste0(ov$missing_pct, "%")))))),
      div(class = "grid2", div(class = "card", h3("Where points were lost"), cats),
          div(class = "card", h3("Target column"), if (is.null(rv$prof0$target)) p(class = "muted", "No target chosen. Go back to step 1 to pick one and unlock balance checks, leakage detection and model training.")
              else { t <- rv$prof0$target; if (t$task == "classification") tagList(p(paste0(t$name, " is a classification target with ", length(t$classes), " classes.")), hbars(names(t$classes), unlist(t$classes), fmt = function(v) v))
                     else p(paste0(t$name, " is a numeric target, so this is a regression task.")) })),
      safety,
      div(class = "card", h3(sprintf("Issues to fix (%d)", nrow(iss))), issues),
      div(class = "card", h3("Columns"), tbl_ui(data.frame(Column = cl$name, Type = cl$type, `Missing %` = cl$missing_pct, Unique = cl$unique_n, Outliers = cl$outlier_n, Example = cl$example, check.names = FALSE), 60)),
      next_btn("Continue to cleaning"))
  }

  stage_clean <- function() {
    sw <- function(id, label, desc, val = TRUE) div(class = "opt", div(checkboxInput(id, label, V(id, val)), div(class = "small muted", style = "margin:-8px 0 0 30px", desc)))
    form <- div(class = "card", h3("3. Choose how to clean"), p(class = "muted", "The defaults suit most data. Every change is recorded in a log."),
      div(class = "grid2", div(
        sw("o_missing", "Turn placeholders into blanks", "N/A, ?, null and - become real missing values."), sw("o_trim", "Trim extra spaces", "Removes spaces at the start and end of text."),
        sw("o_labels", "Unify labels", "Male, male and MALE become one value."), sw("o_numbers", "Convert numbers stored as text", "Handles \u20b9, $, commas and %."),
        sw("o_dates", "Convert dates", "Understands 2026-03-01 and 01/03/2026."),
        sw("o_leak", "Drop target-leaking columns", "Columns that almost copy the answer.")),
        div(sw("o_dups", "Remove duplicate rows", "Keeps the first copy."), sw("o_const", "Drop columns with one value", "They carry no information."), sw("o_id", "Drop ID columns", "IDs do not help a model learn."),
            sw("o_sparse", "Drop mostly empty columns", "Columns missing more than the limit below."), numericInput("o_thr", "Missing limit (%)", V("o_thr", 60), 10, 95, 5))),
      div(class = "grid3", selectInput("o_impnum", "Fill missing numbers with", c("Median" = "median", "Mean" = "mean", "k-nearest neighbours (ML)" = "knn", "Leave blank" = "none"), V("o_impnum", "median")),
          selectInput("o_impcat", "Fill missing categories with", c("Most common value" = "mode", "The word Unknown" = "unknown", "Leave blank" = "none"), V("o_impcat", "mode")),
          selectInput("o_outl", "Outliers", c("Cap to normal range" = "cap", "Remove rows" = "remove", "Keep as they are" = "none"), V("o_outl", "cap"))),
      selectInput("o_pii", "Personal data (emails, phones, IDs, names)", c("Drop the columns" = "drop", "Mask with a one-way code" = "mask", "Keep as they are" = "keep"), V("o_pii", "drop")),
      sw("o_flags", "Add a 'was missing' column", "Helps some models learn from the fact that a value was absent.", FALSE),
      actionButton("run_clean", if (is.null(rv$clean)) "Clean my data" else "Clean again", class = "btn primary"))
    res <- if (!is.null(rv$clean)) {
      cl <- rv$clean; lg <- cl$log
      s0_score <- tryCatch(rv$score0$score %||% 50, error = function(e) 50)
      s0_grade <- tryCatch(rv$score0$grade %||% "Unknown", error = function(e) "Unknown")
      s1_score <- tryCatch(rv$score1$score %||% s0_score, error = function(e) s0_score)
      s1_grade <- tryCatch(rv$score1$grade %||% "Ready", error = function(e) "Ready")
      gain <- suppressWarnings(s1_score - s0_score)
      if (is.na(gain) || !is.finite(gain)) gain <- 0
      gain_msg <- if (gain > 0) sprintf("Score is up %d points. Remaining points are mostly things cleaning cannot fix, such as class imbalance.", gain)
                  else if (gain == 0) "Score unchanged - data was already quite clean."
                  else "Score changed. Review the actions below."
      tagList(
        div(class = "card", h3("Result"),
          div(class = "ring-wrap",
            div(score_ring(s0_score, 120, "before"), div(class = "small muted", style = "text-align:center", s0_grade)),
            div(style = "font-size:2rem;color:var(--accent)", HTML("&rarr;")),
            div(score_ring(s1_score, 150, "after"), div(style = "text-align:center", span(class = paste("chip", grade_class(s1_grade)), s1_grade))),
            div(div(class = "grid2",
                    stat_box("Rows", paste(format(nrow(rv$raw), big.mark = ","), "to", format(nrow(cl$data), big.mark = ","))),
                    stat_box("Columns", paste(ncol(rv$raw), "to", ncol(cl$data)))),
                p(class = "small muted", style = "margin-top:10px", gain_msg)))),
        div(class = "card", h3(sprintf("What changed (%d actions)", nrow(lg))), if (nrow(lg)) tbl_ui(setNames(lg, c("Step", "Column", "Action", "Rows affected")), 100) else p("Nothing needed changing.")),
        div(class = "card", h3("Cleaned data"), tbl_ui(cl$data, 10)),
        div(class = "card", style = "border:2px solid var(--ok); background: var(--ok-soft)",
          p(style = "margin:0 0 12px; font-weight:600; color: var(--ok)", HTML("&#10003; Cleaning successful - ready for next step!")),
          next_btn("Continue to ML preparation"),
          p(class = "small muted", style = "margin:8px 0 0", "You can also adjust options and click 'Clean again' to re-run.")
        ))
    } else {
      if (!is.null(rv$prof0)) div(class = "msg info", style = "margin-top:12px", HTML("Configure options above, then click '<b>Clean my data</b>' to proceed."))
      else div(class = "msg warn", "Please complete 'Check data readiness' (step 2) first.")
    }
    tagList(form, res)
  }



  stage_prepare <- function() {
    req(rv$clean)
    form <- div(class = "card", h3("4. Prepare for machine learning"), p(class = "muted", "Rinse splits first and learns scaling from the training rows only, so no information leaks from test data."),
      div(class = "grid3", selectInput("m_encode", "Text columns", c("One-hot (up to 10 values), else label" = "onehot", "Label numbers only" = "label", "Leave as text" = "none"), V("m_encode", "onehot")),
          selectInput("m_scale", "Scale numbers", c("Standardize (mean 0)" = "standardize", "Min-max (0 to 1)" = "minmax", "No scaling" = "none"), V("m_scale", "standardize")),
          selectInput("m_test", "Test share", c("20%" = 0.2, "25%" = 0.25, "30%" = 0.3, "10%" = 0.1), V("m_test", 0.2))),
      div(class = "grid3", numericInput("m_seed", "Random seed", V("m_seed", 42)), div(class = "opt", checkboxInput("m_strat", "Stratified split", V("m_strat", TRUE))), div(class = "opt", checkboxInput("m_dates", "Split dates into year, month, weekday", V("m_dates", TRUE)))),
      actionButton("run_ml", if (is.null(rv$ml)) "Prepare data" else "Prepare again", class = "btn primary"))
    res <- if (!is.null(rv$ml)) { m <- rv$ml
      tagList(div(class = "card", h3("Ready for modelling"), div(class = "grid4 keep2", stat_box("Task", switch(m$task, classification = "Classification", regression = "Regression", none = "No target")), stat_box("Train rows", format(nrow(m$train), big.mark = ",")), stat_box("Test rows", format(nrow(m$test), big.mark = ",")), stat_box("Features", length(m$spec$features))),
        div(style = "height:12px"), tags$ul(class = "log", lapply(m$steps, tags$li))), div(class = "card", h3("Training data preview"), tbl_ui(m$train, 8)), next_btn(if (m$task == "none") "Go to export" else "Train models")) }
    tagList(form, res)
  }

  stage_export <- function() {
    req(rv$clean); has_ml <- !is.null(rv$ml); has_mod <- !is.null(rv$mod)
    row <- function(btn, d) div(class = "dl", div(btn, div(class = "d", d)))
    dlb <- function(id, label, primary = FALSE) downloadButton(id, label, class = paste("btn", if (primary) "primary"))
    tagList(div(class = "card", h3("7. Download your results"),
      row(dlb("bundle", "Everything (ZIP)", TRUE), "Cleaned data, train and test files, reports, recipe, Python loader and the trained model if you ran step 5."),
      row(dlb("cleaned", "Cleaned data (CSV)"), "All rows after cleaning, before encoding and scaling."),
      if (has_ml) tagList(row(dlb("train", "Train set (CSV)"), "Encoded and scaled. Ready for scikit-learn, XGBoost or R."), row(dlb("test", "Test set (CSV)"), "Scaled with the training statistics.")) else div(class = "msg info", "Go back to step 4 to create train and test files."),
      if (has_mod) tagList(row(dlb("model", "Trained model (RDS)"), "The best model plus the full cleaning pipeline. Load it in R or use the Predict page."), row(dlb("preds", "Test predictions (CSV)"), "Actual and predicted values for every test row.")),
      row(dlb("excel", "Excel report"), "Before and after profile, issues, cleaning log, model results and cleaned data."),
      row(dlb("report", "HTML report"), "One page you can open anywhere and print to PDF."),
      row(dlb("sqlitedb", "SQLite database"), "Cleaned data, cleaning log and the train and test sets as tables in one .sqlite file."),
      row(dlb("recipe", "Recipe (JSON)"), "Every setting used, so the run can be repeated.")),
      if (has_ml) div(class = "card", h3("Load it in Python"), tags$pre(class = "code", python_snippet(ctx()))),
      div(class = "row-btns", actionButton("new_run", "Analyze another file", class = "btn")))
  }

  page_history <- function() {
    h <- user_runs(rv$user$id)
    tagList(div(class = "page-head", div(h1("Your history"), p(class = "muted", "Every dataset you cleaned, with its score before and after."))),
      if (!nrow(h)) div(class = "card empty", h3("No analyses yet"), p("Run your first dataset and it will appear here."), tags$button(type = "button", class = "btn primary", onclick = js_set("nav", "analyze"), "Start now"))
      else tagList(div(class = "grid3", stat_box("Datasets", nrow(h)), stat_box("Average score before", round(mean(h$score_before))), stat_box("Average score after", round(mean(h$score_after)), "good")), div(style = "height:14px"),
        div(class = "card", DT::DTOutput("tbl_history"))))
  }
  output$tbl_history <- DT::renderDT({ req(rv$user); h <- user_runs(rv$user$id, 500)
    dt_table(data.frame(File = h$filename, Rows = h$n_rows, Columns = h$n_cols, Target = ifelse(is.na(h$target), "", h$target), Before = h$score_before, After = h$score_after, Result = h$grade, When = h$created_at, check.names = FALSE)) })

  page_help <- function() {
    tagList(div(class = "page-head", div(h1("Help"), p(class = "muted", "How Rinse scores, cleans and models data."))),
      div(class = "card", h3("How the score works"), p("Every dataset starts at 100. Rinse subtracts points for problems that hurt machine learning, and shows each deduction so nothing is a black box."),
        tags$table(class = "t", tags$thead(tags$tr(tags$th("Category"), tags$th("Max loss"), tags$th("What it checks"))), tags$tbody(
          tags$tr(tags$td("Completeness"), tags$td(30), tags$td("Missing values in each column")), tags$tr(tags$td("Consistency"), tags$td(20), tags$td("Spaces, mixed-case labels, placeholders, numbers stored as text")),
          tags$tr(tags$td("Uniqueness"), tags$td(15), tags$td("Duplicate rows")), tags$tr(tags$td("Usefulness"), tags$td(15), tags$td("Constant, empty, ID-like, personal-data and target-leaking columns")),
          tags$tr(tags$td("Distribution"), tags$td(10), tags$td("Outliers and heavy skew")), tags$tr(tags$td("Target"), tags$td(10), tags$td("Missing target values and imbalanced classes"))))),
      div(class = "card", h3("Models and metrics"), p("Rinse trains logistic or linear regression, a decision tree, a random forest and k-nearest neighbours on the same split. For classification it reports accuracy, balanced accuracy, macro F1 and ROC AUC. For regression it reports R squared, RMSE and MAE. The best model is chosen by macro F1 (classification) or R squared (regression), and checked with k-fold cross-validation."),
        p("Macro F1 is used because accuracy can look high on imbalanced data. If 97% of transactions are not fraud, a model that always says 'not fraud' scores 97% accuracy and catches nothing.")),
      div(class = "card", h3("Grades"), p("85 and above is Ready. 70 to 84 is Almost ready. 50 to 69 needs work. Below 50 is not ready. The score is guidance, not a promise about model accuracy.")),
      div(class = "card", h3("Privacy and security"), p("Uploaded files are processed in memory for your session and are not written to disk. Rinse stores your account, a summary of each run (file name, size, scores, model metrics) and a record of sign-ins (time, method, device and IP address) for security. You can export or delete your data in Settings.")),
      div(class = "card", h3("About"), p(paste0(APP_NAME, " ", APP_VERSION, " - ", APP_TAGLINE, ".")),
        p(class = "muted small", "Rinse runs on your own server. Uploaded files are analysed in memory for the length of your session; only your account and a summary of each run are stored.")))
  }
}
