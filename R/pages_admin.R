# Administration console (members, live activity, sign-ins, database browser, system) and user Settings.
APP_STARTED <- Sys.time()
ADMIN_TABS <- c(overview = "Overview", members = "Members", online = "Online now", signins = "Sign-ins", analyses = "Analyses", models = "Models", activity = "Activity", database = "Database", system = "System")

register_admin <- function(input, output, session, rv, safe, do_logout) {
  is_admin <- function() identical(rv$user$role, "admin")
  ver <- reactiveVal(0); bump <- function() ver(ver() + 1)
  tick <- reactiveTimer(15000)
  observeEvent(input$adm_tab, { if (is_admin()) rv$adm_tab <- input$adm_tab })

  # ---------------- tables ----------------
  members <- reactive({ ver(); admin_users() })
  output$tbl_members <- DT::renderDT({ req(is_admin()); u <- members()
    dt_table(data.frame(ID = u$id, Name = u$name, Email = ifelse(is.na(u$email), "", u$email), Phone = ifelse(is.na(u$phone), "", u$phone), Role = u$role, `Sign-in` = u$provider,
                        Active = ifelse(u$active == 1, "Yes", "No"), Logins = u$login_count, Analyses = u$analyses, Joined = u$created_at, `Last login` = u$last_login, check.names = FALSE), select = TRUE) })
  output$tbl_online <- DT::renderDT({ req(is_admin()); tick(); o <- admin_online(); dt_table(setNames(o, c("Name", "Account", "Device", "IP address", "Page", "Started", "Last seen")), filter = FALSE, buttons = FALSE) })
  output$tbl_signins <- DT::renderDT({ req(is_admin()); ver(); d <- admin_login_events(); names(d) <- c("Time", "Account", "Method", "Result", "Reason", "IP address", "Device"); dt_table(d) })
  output$tbl_sessions <- DT::renderDT({ req(is_admin()); d <- admin_sessions(); names(d) <- c("Started", "Account", "Device", "IP address", "Last seen", "Ended"); dt_table(d) })
  output$tbl_analyses <- DT::renderDT({ req(is_admin()); ver(); d <- admin_runs(); names(d) <- c("ID", "Member", "File", "Rows", "Columns", "Target", "Before", "After", "Result", "When"); dt_table(d) })
  output$tbl_models <- DT::renderDT({ req(is_admin()); d <- admin_models(); names(d) <- c("When", "Member", "File", "Task", "Model", "Metric", "Value", "Seconds"); dt_table(d) })
  output$tbl_activity <- DT::renderDT({ req(is_admin()); ver(); d <- admin_audit(); names(d) <- c("Time", "Who", "Event", "Detail"); dt_table(d) })
  output$tbl_dbview <- DT::renderDT({ req(is_admin(), input$db_table); dt_table(db_read_table(input$db_table)) })
  output$tbl_dbsum <- renderUI({ req(is_admin()); ver(); st <- db_stats(); tagList(div(class = "grid3", stat_box("Database size", paste(st$size_kb, "KB")), stat_box("Schema version", st$version), stat_box("Tables", nrow(st$tables))), div(style = "height:12px"), tbl_ui(st$tables, 30)) })

  output$adm_overview <- renderUI({
    req(is_admin()); tick(); ver(); st <- admin_stats(); on <- admin_online(); tr <- admin_login_trend(14); rec <- utils::head(admin_login_events(8), 8)
    tagList(div(class = "grid4 keep2", stat_box("Members", st$users), stat_box("Online now", st$online, if (st$online) "good"), stat_box("Sign-ins (24h)", st$logins_24h), stat_box("Failed sign-ins (24h)", st$failed_24h, if (st$failed_24h) "bad")),
      div(style = "height:14px"), div(class = "grid4 keep2", stat_box("Analyses", st$runs), stat_box("Analyses today", st$today), stat_box("Models trained", st$models), stat_box("Average score gain", paste0("+", st$avg_gain))), div(style = "height:18px"),
      div(class = "grid2", div(class = "card", h3("Sign-ins, last 14 days"), svg_trend(tr)),
          div(class = "card", h3(tags$span(class = "dot"), "Online now"), if (nrow(on)) tbl_ui(setNames(on[, c("name", "account", "device", "page", "last_seen")], c("Name", "Account", "Device", "Page", "Last seen")), 10) else p(class = "muted", "Nobody is active right now."))),
      div(class = "card", h3("Latest sign-in attempts"), tbl_ui(setNames(rec[, c("time", "identifier", "method", "result", "ip", "device")], c("Time", "Account", "Method", "Result", "IP address", "Device")), 8)))
  })

  output$log_tail <- renderText({ req(is_admin()); tick(); f <- log_file(); if (file.exists(f)) paste(utils::tail(readLines(f, warn = FALSE), 60), collapse = "\n") else "No log yet." })
  output$backup_list <- renderUI({ req(is_admin()); ver(); d <- file.path(dirname(CFG$db_path), "backups"); f <- if (dir.exists(d)) sort(list.files(d, pattern = "\\.sqlite$"), decreasing = TRUE) else character()
    if (length(f)) tbl_ui(data.frame(Backup = f, `Size (KB)` = round(file.size(file.path(d, f)) / 1024, 1), check.names = FALSE), 10) else p(class = "muted", "No backups yet.") })
  output$db_download <- downloadHandler(filename = function() paste0("rinse_backup_", format(Sys.time(), "%Y%m%d_%H%M"), ".sqlite"), content = function(f) { req(is_admin()); db_backup(f); audit(rv$user$id, "db_download") })
  observeEvent(input$db_backup_now, { req(is_admin()); safe({ p <- db_backup(); audit(rv$user$id, "db_backup", basename(p)); showNotification(paste("Backup saved:", basename(p))); bump() }) })

  # ---------------- member actions ----------------
  sel_id <- reactive({ i <- input$tbl_members_rows_selected; if (length(i) != 1) NULL else members()$id[i] })
  need_sel <- function() { id <- sel_id(); if (is.null(id)) { showNotification("Select a member in the table first.", type = "warning"); NULL } else id }
  act <- function(fn, msg) { req(is_admin()); id <- need_sel(); if (is.null(id)) return(); safe({ fn(id); showNotification(msg); bump() }) }
  observeEvent(input$adm_off, act(function(id) admin_set_active(id, FALSE, rv$user$id), "Member disabled and signed out."))
  observeEvent(input$adm_on, act(function(id) admin_set_active(id, TRUE, rv$user$id), "Member enabled."))
  observeEvent(input$adm_out, act(function(id) admin_force_logout(id, rv$user$id), "Member signed out everywhere."))
  observeEvent(input$adm_mkadmin, act(function(id) admin_set_role(id, "admin", rv$user$id), "Role changed to administrator."))
  observeEvent(input$adm_mkuser, act(function(id) admin_set_role(id, "user", rv$user$id), "Role changed to member."))
  observeEvent(input$adm_reset, { req(is_admin()); id <- need_sel(); if (is.null(id)) return()
    safe({ tmp <- admin_reset_password(id, rv$user$id); bump(); showModal(modalDialog(title = "Temporary password", p("Give this to the member. It is shown only once and they must change it after signing in."), tags$pre(class = "code", style = "font-size:1.2rem", tmp), footer = modalButton("Done"), easyClose = TRUE)) }) })
  observeEvent(input$adm_del, { req(is_admin()); id <- need_sel(); if (is.null(id)) return()
    showModal(modalDialog(title = "Delete this member?", "Their account, analyses and sign-in sessions are removed permanently.", footer = tagList(modalButton("Cancel"), actionButton("adm_del_ok", "Delete", class = "btn danger")), easyClose = TRUE)) })
  observeEvent(input$adm_del_ok, { req(is_admin()); id <- sel_id(); req(id); removeModal(); if (admin_delete_user(id, rv$user$id)) showNotification("Member deleted.") else showNotification("The primary administrator cannot be deleted.", type = "error"); bump() })

  # ---------------- Admin page ----------------
  page_admin <- function() {
    tab <- rv$adm_tab %||% "overview"
    tabs <- div(class = "tabs2", lapply(names(ADMIN_TABS), function(k) nav_btn("adm_tab", k, ADMIN_TABS[[k]], tab == k)))
    body <- switch(tab,
      overview = uiOutput("adm_overview"),
      members = div(class = "card", h3("Members"), p(class = "muted small", "Click a row to select a member, then use the buttons."), DT::DTOutput("tbl_members"),
        div(class = "row-btns", style = "margin-top:14px", actionButton("adm_off", "Disable", class = "btn sm"), actionButton("adm_on", "Enable", class = "btn sm"), actionButton("adm_out", "Sign out everywhere", class = "btn sm"),
            actionButton("adm_reset", "Reset password", class = "btn sm"), actionButton("adm_mkadmin", "Make admin", class = "btn sm"), actionButton("adm_mkuser", "Make member", class = "btn sm"), actionButton("adm_del", "Delete", class = "btn sm danger"))),
      online = div(class = "card", h3(tags$span(class = "dot"), "Signed in right now"), p(class = "muted small", "People active in the last two minutes. Refreshes every 15 seconds."), DT::DTOutput("tbl_online"),
                   div(style = "height:18px"), h3("All sessions"), DT::DTOutput("tbl_sessions")),
      signins = div(class = "card", h3("Sign-in history"), p(class = "muted small", "Every successful and failed attempt with time, method, device and IP address."), DT::DTOutput("tbl_signins")),
      analyses = div(class = "card", h3("Analyses"), DT::DTOutput("tbl_analyses")),
      models = div(class = "card", h3("Model training runs"), DT::DTOutput("tbl_models")),
      activity = div(class = "card", h3("Activity log"), p(class = "muted small", "Account changes, data loads, cleaning, training and administrator actions."), DT::DTOutput("tbl_activity")),
      database = tagList(div(class = "card", h3("Database overview"), uiOutput("tbl_dbsum")),
        div(class = "card", h3("Table browser"), p(class = "muted small", "Read-only. Password hashes and secrets are never shown."), selectInput("db_table", "Table", db_tables(), input$db_table %||% "users"), DT::DTOutput("tbl_dbview"))),
      system = tagList(div(class = "card", h3("System"), div(class = "grid3", stat_box("App version", APP_VERSION), stat_box("R version", paste(R.version$major, R.version$minor, sep = ".")), stat_box("Up since", format(APP_STARTED, "%d %b %H:%M"))),
          div(style = "height:12px"), tbl_ui(data.frame(Setting = c("Database", "Schema version", "Upload limit", "Sign-in lockout", "Idle sign-out", "Google sign-in", "SMS sign-in", "Log level"),
            Value = c(CFG$db_path, db_version(), paste(CFG$max_upload_mb, "MB"), sprintf("%d attempts, %d minutes", CFG$max_fail, CFG$lock_minutes), paste(CFG$idle_minutes, "minutes"), if (CFG$google_on) "On" else "Off", if (CFG$sms_on) "On (Twilio)" else "Demo mode", CFG$log_level)), 20)),
        div(class = "card", h3("Backups"), p(class = "muted small", "A backup is a complete copy of the database. Make one before upgrades and on a schedule (see scripts/backup.sh)."),
            div(class = "row-btns", actionButton("db_backup_now", "Create backup now", class = "btn primary"), downloadButton("db_download", "Download a backup", class = "btn")), div(style = "height:12px"), uiOutput("backup_list")),
        div(class = "card", h3("Application log"), p(class = "muted small", "Last 60 lines. Refreshes every 15 seconds."), tags$pre(class = "code", style = "max-height:360px", textOutput("log_tail", inline = TRUE)))))
    tagList(div(class = "page-head", div(h1("Administration"), p(class = "muted", "Members, live activity, sign-ins and the database."))), tabs, body)
  }

  # ---------------- Settings page ----------------
  observeEvent(input$st_save, { req(rv$user); r <- update_profile(rv$user$id, input$st_name)
    if (r$ok) { rv$user$name <- trimws(input$st_name); showNotification("Profile saved.") } else showNotification(r$msg, type = "error") })
  observeEvent(input$pw_save, { req(rv$user); r <- change_password(rv$user$id, input$pw_old %||% "", input$pw_new %||% "")
    if (r$ok) { rv$user$must_change <- FALSE; showNotification("Password changed. Other devices were signed out."); audit(rv$user$id, "pw_change_ui") } else showNotification(r$msg, type = "error") })
  output$my_data <- downloadHandler(filename = function() "my_rinse_data.json", content = function(f) { req(rv$user); writeLines(jsonlite::toJSON(export_user_data(rv$user$id), auto_unbox = TRUE, pretty = TRUE, na = "null"), f) })
  observeEvent(input$acct_del, { req(rv$user); if (identical(rv$user$role, "admin")) { showNotification("Administrator accounts cannot be deleted here.", type = "error"); return() }
    showModal(modalDialog(title = "Delete your account?", p("This permanently removes your account, history and sign-in records. Type DELETE to confirm."), textInput("acct_confirm", NULL, placeholder = "DELETE"),
      footer = tagList(modalButton("Cancel"), actionButton("acct_del_ok", "Delete my account", class = "btn danger")), easyClose = TRUE)) })
  observeEvent(input$acct_del_ok, { req(rv$user); if (!identical(trimws(input$acct_confirm), "DELETE")) { showNotification("Type DELETE exactly to confirm.", type = "error"); return() }
    r <- delete_account(rv$user$id); removeModal(); if (r$ok) do_logout("Your account was deleted.") else showNotification(r$msg, type = "error") })
  output$tbl_mysignins <- renderUI({ req(rv$user); d <- q_get("SELECT ts, method, success, ip, device FROM login_events WHERE user_id = ? ORDER BY id DESC LIMIT 10", list(rv$user$id))
    tbl_ui(data.frame(Time = d$ts, Method = d$method, Result = ifelse(d$success == 1, "Success", "Failed"), `IP address` = d$ip, Device = d$device, check.names = FALSE), 10) })

  page_settings <- function() {
    u <- rv$user
    tagList(div(class = "page-head", div(h1("Settings"), p(class = "muted", "Your profile, security and data."))),
      div(class = "grid2", div(class = "card", h3("Profile"), textInput("st_name", "Name", u$name), div(class = "help", paste("Signed in with", u$provider, "as", u$email %||% u$phone)),
            actionButton("st_save", "Save profile", class = "btn primary")),
          div(class = "card", h3("Password"), if (identical(u$provider, "email")) tagList(if (isTRUE(u$must_change)) div(class = "msg info", "Enter the temporary password you were given as the current password."),
              passwordInput("pw_old", "Current password"), passwordInput("pw_new", "New password"), div(class = "help", "At least 8 characters with a letter and a number. Changing it signs out your other devices."), actionButton("pw_save", "Change password", class = "btn primary"))
            else p(class = "muted", paste("You sign in with", u$provider, "so there is no password to change here.")))),
      div(class = "card", h3("Recent sign-ins to your account"), p(class = "muted small", "If you see a sign-in you do not recognise, change your password."), uiOutput("tbl_mysignins")),
      div(class = "card", h3("Your data"), p(class = "muted", "Download everything Rinse stores about you, or delete your account."), div(class = "row-btns", downloadButton("my_data", "Download my data (JSON)", class = "btn"), if (!identical(u$role, "admin")) actionButton("acct_del", "Delete my account", class = "btn danger"))))
  }
  list(page_admin = page_admin, page_settings = page_settings)
}
