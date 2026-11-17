# Authentication: email + password, phone OTP, Google OAuth 2.0, roles, lockout, login tracking.
# Passwords are never stored in plain text. They are hashed with Argon2 (libsodium).
hash_pw   <- function(pw) sodium::password_store(pw)
check_pw  <- function(pw, h) isTRUE(tryCatch(sodium::password_verify(h, pw), error = function(e) FALSE))
sha       <- function(x) sodium::bin2hex(sodium::sha256(charToRaw(x)))
valid_email <- function(e) grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", e)

norm_phone <- function(p) {
  p <- gsub("[^0-9+]", "", p)
  if (grepl("^[6-9][0-9]{9}$", p)) p <- paste0("+91", p)
  if (grepl("^91[6-9][0-9]{9}$", p)) p <- paste0("+", p)
  if (grepl("^\\+[1-9][0-9]{9,14}$", p)) p else NA_character_
}

password_problem <- function(pw) {
  if (nchar(pw) < 8) return("Use at least 8 characters.")
  if (!grepl("[A-Za-z]", pw) || !grepl("[0-9]", pw)) return("Include at least one letter and one number.")
  NULL
}

get_user <- function(id) { u <- q_get("SELECT * FROM users WHERE id = ?", list(id)); if (nrow(u)) u[1, ] else NULL }
public_user <- function(u) list(id = u$id, email = u$email, phone = u$phone, name = u$name, role = u$role, provider = u$provider,
                                theme = u$theme %||% "light", must_change = isTRUE(u$must_change == 1))

# ---- Request context (IP and device) ----
parse_device <- function(ua) {
  ua <- ua %||% ""
  os <- if (grepl("Android", ua)) "Android" else if (grepl("iPhone|iPad|iOS", ua)) "iOS" else if (grepl("Windows", ua)) "Windows" else if (grepl("Mac OS|Macintosh", ua)) "macOS" else if (grepl("Linux", ua)) "Linux" else "Unknown"
  br <- if (grepl("Edg/", ua)) "Edge" else if (grepl("OPR/|Opera", ua)) "Opera" else if (grepl("Chrome/", ua)) "Chrome" else if (grepl("Firefox/", ua)) "Firefox" else if (grepl("Safari/", ua)) "Safari" else "Other"
  kind <- if (grepl("Mobile|Android|iPhone", ua)) "Mobile" else "Desktop"
  paste0(kind, " / ", os, " / ", br)
}
request_ctx <- function(session) {
  r <- tryCatch(session$request, error = function(e) NULL)
  g <- function(k) { v <- tryCatch(r[[k]], error = function(e) NULL); if (is.null(v) || !length(v)) "" else as.character(v)[1] }
  fwd <- trimws(strsplit(g("HTTP_X_FORWARDED_FOR"), ",")[[1]][1] %||% "")
  ip <- if (nzchar(fwd)) fwd else if (nzchar(g("REMOTE_ADDR"))) g("REMOTE_ADDR") else "unknown"
  ua <- g("HTTP_USER_AGENT")
  list(ip = ip, ua = ua, device = parse_device(ua))
}

log_login <- function(user_id, ident, method, ok, reason = "", ctx = NULL) {
  ctx <- ctx %||% list(ip = "", ua = "", device = "")
  tryCatch({
    q_exec("INSERT INTO login_events (ts, user_id, identifier, method, success, reason, ip, device, user_agent) VALUES (?,?,?,?,?,?,?,?,?)",
           list(now_ts(), if (is.null(user_id)) NA_integer_ else user_id, ident, method, as.integer(ok), reason, ctx$ip, ctx$device, substr(ctx$ua, 1, 250)))
    if (ok && !is.null(user_id)) q_exec("UPDATE users SET login_count = COALESCE(login_count,0) + 1 WHERE id = ?", list(user_id))
    if (!ok) vlog("WARN", "Failed %s sign-in for %s from %s (%s)", method, ident, ctx$ip, reason)
  }, error = function(e) NULL)
}

register_user <- function(name, email, password, ctx = NULL) {
  email <- tolower(trimws(email)); name <- trimws(name)
  if (!nzchar(name)) return(list(ok = FALSE, msg = "Enter your name."))
  if (!valid_email(email)) return(list(ok = FALSE, msg = "Enter a valid email address."))
  pp <- password_problem(password); if (!is.null(pp)) return(list(ok = FALSE, msg = pp))
  if (nrow(q_get("SELECT id FROM users WHERE email = ?", list(email)))) return(list(ok = FALSE, msg = "An account with this email already exists. Sign in instead."))
  q_exec("INSERT INTO users (email, name, pw_hash, provider, created_at, last_login) VALUES (?,?,?,?,?,?)", list(email, name, hash_pw(password), "email", now_ts(), now_ts()))
  u <- q_get("SELECT * FROM users WHERE email = ?", list(email))[1, ]
  audit(u$id, "register", email); log_login(u$id, email, "signup", TRUE, "", ctx)
  list(ok = TRUE, user = public_user(u))
}

login_email <- function(email, password, ctx = NULL) {
  email <- tolower(trimws(email)); generic <- "Email or password is incorrect."
  if (!valid_email(email) || !nzchar(password)) return(list(ok = FALSE, msg = generic))
  u <- q_get("SELECT * FROM users WHERE email = ?", list(email))
  if (!nrow(u)) { sodium::password_store("x"); log_login(NULL, email, "email", FALSE, "unknown account", ctx); return(list(ok = FALSE, msg = generic)) }
  u <- u[1, ]
  if (!is.na(u$locked_until) && nzchar(u$locked_until) && u$locked_until > now_ts()) {
    log_login(u$id, email, "email", FALSE, "locked", ctx)
    return(list(ok = FALSE, msg = sprintf("Too many attempts. Try again after %s.", substr(u$locked_until, 12, 16))))
  }
  if (!isTRUE(u$active == 1)) { log_login(u$id, email, "email", FALSE, "disabled", ctx); return(list(ok = FALSE, msg = "This account is disabled. Contact the administrator.")) }
  if (is.na(u$pw_hash) || !check_pw(password, u$pw_hash)) {
    f <- u$fail_count + 1L
    lock <- if (f >= CFG$max_fail) format(Sys.time() + CFG$lock_minutes * 60, "%Y-%m-%d %H:%M:%S") else NA_character_
    q_exec("UPDATE users SET fail_count = ?, locked_until = ? WHERE id = ?", list(if (f >= CFG$max_fail) 0L else f, lock, u$id))
    audit(u$id, "login_failed", email); log_login(u$id, email, "email", FALSE, if (!is.na(lock)) "wrong password, locked" else "wrong password", ctx)
    return(list(ok = FALSE, msg = if (!is.na(lock)) sprintf("Too many attempts. Account locked for %d minutes.", CFG$lock_minutes) else generic))
  }
  q_exec("UPDATE users SET fail_count = 0, locked_until = NULL, last_login = ? WHERE id = ?", list(now_ts(), u$id))
  audit(u$id, "login", "email"); log_login(u$id, email, "email", TRUE, "", ctx)
  list(ok = TRUE, user = public_user(u))
}

# ---- Phone OTP ----
send_otp <- function(phone_raw) {
  phone <- norm_phone(phone_raw)
  if (is.na(phone)) return(list(ok = FALSE, msg = "Enter a valid mobile number, for example 9876543210."))
  recent <- q_get("SELECT COUNT(*) AS n FROM otps WHERE phone = ? AND created_at > ?", list(phone, ago_ts(600)))$n
  if (recent >= 3) return(list(ok = FALSE, msg = "Too many codes requested. Wait 10 minutes and try again."))
  code <- sprintf("%06d", sum(as.integer(sodium::random(3)) * c(65536, 256, 1)) %% 1000000)
  q_exec("INSERT INTO otps (phone, code_hash, expires_at, created_at) VALUES (?,?,?,?)",
         list(phone, sha(paste0(code, phone)), format(Sys.time() + CFG$otp_minutes * 60, "%Y-%m-%d %H:%M:%S"), now_ts()))
  if (CFG$sms_on) {
    r <- tryCatch(httr::POST(sprintf("https://api.twilio.com/2010-04-01/Accounts/%s/Messages.json", CFG$twilio_sid),
                  httr::authenticate(CFG$twilio_sid, CFG$twilio_token), httr::timeout(15),
                  body = list(To = phone, From = CFG$twilio_from, Body = sprintf("Your %s code is %s. It expires in %d minutes.", APP_NAME, code, CFG$otp_minutes))),
                  error = function(e) NULL)
    if (is.null(r) || httr::status_code(r) >= 300) { vlog("ERROR", "SMS delivery failed for %s", phone); return(list(ok = FALSE, msg = "Could not send the SMS. Try again or use email sign-in.")) }
    return(list(ok = TRUE, phone = phone, demo = FALSE))
  }
  list(ok = TRUE, phone = phone, demo = TRUE, code = code)   # demo mode: no SMS provider configured
}

verify_otp <- function(phone, code, ctx = NULL) {
  o <- q_get("SELECT * FROM otps WHERE phone = ? ORDER BY id DESC LIMIT 1", list(phone))
  if (!nrow(o)) return(list(ok = FALSE, msg = "Request a code first."))
  o <- o[1, ]
  if (o$expires_at < now_ts()) return(list(ok = FALSE, msg = "That code has expired. Request a new one."))
  if (o$attempts >= 5) return(list(ok = FALSE, msg = "Too many wrong codes. Request a new one."))
  if (!identical(sha(paste0(trimws(code), phone)), o$code_hash)) {
    q_exec("UPDATE otps SET attempts = attempts + 1 WHERE id = ?", list(o$id)); log_login(NULL, phone, "phone", FALSE, "wrong code", ctx)
    return(list(ok = FALSE, msg = "That code is not correct."))
  }
  q_exec("DELETE FROM otps WHERE phone = ?", list(phone))
  u <- q_get("SELECT * FROM users WHERE phone = ?", list(phone))
  if (!nrow(u)) {
    q_exec("INSERT INTO users (phone, name, provider, created_at, last_login) VALUES (?,?,?,?,?)", list(phone, paste("User", substr(phone, nchar(phone) - 3, nchar(phone))), "phone", now_ts(), now_ts()))
    u <- q_get("SELECT * FROM users WHERE phone = ?", list(phone))
  }
  u <- u[1, ]
  if (!isTRUE(u$active == 1)) { log_login(u$id, phone, "phone", FALSE, "disabled", ctx); return(list(ok = FALSE, msg = "This account is disabled. Contact the administrator.")) }
  q_exec("UPDATE users SET last_login = ? WHERE id = ?", list(now_ts(), u$id))
  audit(u$id, "login", "phone"); log_login(u$id, phone, "phone", TRUE, "", ctx)
  list(ok = TRUE, user = public_user(u))
}

# ---- Google OAuth 2.0 (authorization code flow) ----
google_url <- function(state) {
  paste0("https://accounts.google.com/o/oauth2/v2/auth?response_type=code&scope=", utils::URLencode("openid email profile", reserved = TRUE),
         "&client_id=", utils::URLencode(CFG$google_id, reserved = TRUE), "&redirect_uri=", utils::URLencode(CFG$google_redirect, reserved = TRUE),
         "&state=", state, "&prompt=select_account")
}

google_login <- function(code, ctx = NULL) {
  tk <- tryCatch(httr::POST("https://oauth2.googleapis.com/token", encode = "form", httr::timeout(15),
         body = list(code = code, client_id = CFG$google_id, client_secret = CFG$google_secret, redirect_uri = CFG$google_redirect, grant_type = "authorization_code")), error = function(e) NULL)
  if (is.null(tk) || httr::status_code(tk) != 200) { log_login(NULL, "google", "google", FALSE, "token exchange failed", ctx); return(list(ok = FALSE, msg = "Google sign-in failed. Please try again.")) }
  at <- httr::content(tk, as = "parsed")$access_token
  ui <- tryCatch(httr::GET("https://www.googleapis.com/oauth2/v3/userinfo", httr::add_headers(Authorization = paste("Bearer", at)), httr::timeout(15)), error = function(e) NULL)
  if (is.null(ui) || httr::status_code(ui) != 200) return(list(ok = FALSE, msg = "Could not read your Google profile."))
  p <- httr::content(ui, as = "parsed")
  if (!isTRUE(p$email_verified)) return(list(ok = FALSE, msg = "Your Google email is not verified."))
  email <- tolower(p$email)
  u <- q_get("SELECT * FROM users WHERE email = ?", list(email))
  if (!nrow(u)) {
    q_exec("INSERT INTO users (email, name, provider, created_at, last_login) VALUES (?,?,?,?,?)", list(email, p$name %||% email, "google", now_ts(), now_ts()))
    u <- q_get("SELECT * FROM users WHERE email = ?", list(email))
  }
  u <- u[1, ]
  if (!isTRUE(u$active == 1)) { log_login(u$id, email, "google", FALSE, "disabled", ctx); return(list(ok = FALSE, msg = "This account is disabled. Contact the administrator.")) }
  q_exec("UPDATE users SET last_login = ? WHERE id = ?", list(now_ts(), u$id)); audit(u$id, "login", "google"); log_login(u$id, email, "google", TRUE, "", ctx)
  list(ok = TRUE, user = public_user(u))
}

# ---- Remember-me sessions (random token in the browser, only its hash in the database) ----
create_session <- function(user_id, days = 14) {
  tok <- sodium::bin2hex(sodium::random(32))
  q_exec("DELETE FROM sessions WHERE expires_at < ?", list(now_ts()))
  q_exec("INSERT INTO sessions (token_hash, user_id, expires_at) VALUES (?,?,?)", list(sha(tok), user_id, format(Sys.time() + days * 86400, "%Y-%m-%d %H:%M:%S")))
  tok
}
session_user <- function(tok) {
  if (is.null(tok) || !nzchar(tok)) return(NULL)
  s <- q_get("SELECT user_id FROM sessions WHERE token_hash = ? AND expires_at > ?", list(sha(tok), now_ts()))
  if (!nrow(s)) return(NULL)
  u <- get_user(s$user_id[1]); if (is.null(u) || !isTRUE(u$active == 1)) return(NULL)
  public_user(u)
}
end_session <- function(tok) if (!is.null(tok) && nzchar(tok)) q_exec("DELETE FROM sessions WHERE token_hash = ?", list(sha(tok)))

# ---- Live activity tracking (who is online) ----
track_start <- function(user_id, ctx, page = "analyze") {
  sid <- sodium::bin2hex(sodium::random(12))
  q_exec("INSERT INTO user_sessions (id, user_id, started, last_seen, ip, device, page) VALUES (?,?,?,?,?,?,?)", list(sid, user_id, now_ts(), now_ts(), ctx$ip, ctx$device, page))
  sid
}
track_beat <- function(sid, page = NULL) if (!is.null(sid)) tryCatch(q_exec("UPDATE user_sessions SET last_seen = ?, page = COALESCE(?, page) WHERE id = ? AND ended IS NULL", list(now_ts(), if (is.null(page)) NA_character_ else page, sid)), error = function(e) NULL)
track_end <- function(sid) if (!is.null(sid)) tryCatch(q_exec("UPDATE user_sessions SET ended = ? WHERE id = ? AND ended IS NULL", list(now_ts(), sid)), error = function(e) NULL)

# ---- Account self-service ----
change_password <- function(id, old, new) {
  u <- get_user(id); if (is.null(u)) return(list(ok = FALSE, msg = "Account not found."))
  if (!is.na(u$pw_hash) && !isTRUE(u$must_change == 1) && !check_pw(old, u$pw_hash)) return(list(ok = FALSE, msg = "Your current password is not correct."))
  if (isTRUE(u$must_change == 1) && !is.na(u$pw_hash) && !check_pw(old, u$pw_hash)) return(list(ok = FALSE, msg = "Enter the temporary password you were given."))
  pp <- password_problem(new); if (!is.null(pp)) return(list(ok = FALSE, msg = pp))
  q_exec("UPDATE users SET pw_hash = ?, must_change = 0 WHERE id = ?", list(hash_pw(new), id)); q_exec("DELETE FROM sessions WHERE user_id = ?", list(id))
  audit(id, "password_changed"); list(ok = TRUE)
}
update_profile <- function(id, name) {
  name <- trimws(name); if (!nzchar(name)) return(list(ok = FALSE, msg = "Name cannot be empty."))
  q_exec("UPDATE users SET name = ? WHERE id = ?", list(name, id)); list(ok = TRUE)
}
export_user_data <- function(id) {
  list(profile = q_get("SELECT id, email, phone, name, role, provider, created_at, last_login, login_count FROM users WHERE id = ?", list(id)),
       analyses = q_get("SELECT * FROM runs WHERE user_id = ?", list(id)), models = q_get("SELECT * FROM model_runs WHERE user_id = ?", list(id)),
       sign_ins = q_get("SELECT ts, method, success, ip, device FROM login_events WHERE user_id = ? ORDER BY id DESC LIMIT 500", list(id)))
}
delete_account <- function(id) {
  u <- get_user(id); if (is.null(u) || u$role == "admin") return(list(ok = FALSE, msg = "Administrator accounts cannot be deleted here."))
  for (t in c("runs", "model_runs", "sessions", "user_sessions")) q_exec(sprintf("DELETE FROM %s WHERE user_id = ?", t), list(id))
  q_exec("DELETE FROM users WHERE id = ?", list(id)); audit(NULL, "account_deleted", as.character(id)); list(ok = TRUE)
}

# ---- Signed OAuth state (stops forged Google callbacks) ----
app_secret <- function() {
  s <- env("RINSE_SECRET"); if (nzchar(s)) return(charToRaw(s))
  f <- file.path(dirname(CFG$db_path), ".secret")
  if (!file.exists(f)) { dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE); writeLines(sodium::bin2hex(sodium::random(32)), f) }
  charToRaw(readLines(f, n = 1, warn = FALSE))
}
make_state <- function() { t <- as.character(as.integer(Sys.time())); paste0(t, "-", digest::hmac(app_secret(), t, "sha256")) }
check_state <- function(st) {
  p <- strsplit(st, "-", fixed = TRUE)[[1]]; if (length(p) != 2 || !grepl("^[0-9]+$", p[1])) return(FALSE)
  age <- as.integer(Sys.time()) - as.integer(p[1])
  age >= 0 && age < 600 && identical(p[2], digest::hmac(app_secret(), p[1], "sha256"))
}

# ---- Admin ----
admin_users <- function() q_get("SELECT u.id, u.name, u.email, u.phone, u.role, u.provider, u.active, u.login_count, u.created_at, u.last_login, (SELECT COUNT(*) FROM runs r WHERE r.user_id = u.id) AS analyses FROM users u ORDER BY u.id")
admin_set_active <- function(id, active, by) { q_exec("UPDATE users SET active = ? WHERE id = ? AND email != ?", list(as.integer(active), id, CFG$admin_email)); if (!active) { q_exec("DELETE FROM sessions WHERE user_id = ?", list(id)); admin_force_logout(id, by, quiet = TRUE) }; audit(by, if (active) "enable_user" else "disable_user", as.character(id)) }
admin_set_role <- function(id, role, by) { if (!role %in% c("user", "admin")) return(FALSE); q_exec("UPDATE users SET role = ? WHERE id = ? AND email != ?", list(role, id, CFG$admin_email)); audit(by, "set_role", paste(id, role)); TRUE }
admin_force_logout <- function(id, by, quiet = FALSE) { q_exec("DELETE FROM sessions WHERE user_id = ?", list(id)); q_exec("UPDATE user_sessions SET ended = ? WHERE user_id = ? AND ended IS NULL", list(now_ts(), id)); if (!quiet) audit(by, "force_logout", as.character(id)) }
admin_reset_password <- function(id, by) {
  tmp <- paste0(sample(c(letters, LETTERS), 6, TRUE) |> paste(collapse = ""), sprintf("%03d", sample(100:999, 1)))
  q_exec("UPDATE users SET pw_hash = ?, must_change = 1, fail_count = 0, locked_until = NULL WHERE id = ?", list(hash_pw(tmp), id)); q_exec("DELETE FROM sessions WHERE user_id = ?", list(id))
  audit(by, "reset_password", as.character(id)); tmp
}
admin_delete_user <- function(id, by) { u <- get_user(id); if (!is.null(u) && u$email %in% CFG$admin_email) return(FALSE); delete_account_force(id); audit(by, "delete_user", as.character(id)); TRUE }
delete_account_force <- function(id) { for (t in c("runs", "model_runs", "sessions", "user_sessions")) q_exec(sprintf("DELETE FROM %s WHERE user_id = ?", t), list(id)); q_exec("DELETE FROM users WHERE id = ?", list(id)) }
admin_stats <- function() list(
  users = q_get("SELECT COUNT(*) n FROM users")$n, runs = q_get("SELECT COUNT(*) n FROM runs")$n,
  avg_gain = round(q_get("SELECT COALESCE(AVG(score_after - score_before),0) g FROM runs")$g, 1),
  today = q_get("SELECT COUNT(*) n FROM runs WHERE created_at >= ?", list(format(Sys.Date(), "%Y-%m-%d")))$n,
  online = q_get("SELECT COUNT(DISTINCT user_id) n FROM user_sessions WHERE ended IS NULL AND last_seen > ?", list(ago_ts(120)))$n,
  logins_24h = q_get("SELECT COUNT(*) n FROM login_events WHERE success = 1 AND ts > ?", list(ago_ts(86400)))$n,
  failed_24h = q_get("SELECT COUNT(*) n FROM login_events WHERE success = 0 AND ts > ?", list(ago_ts(86400)))$n,
  models = q_get("SELECT COUNT(*) n FROM model_runs")$n)
admin_online <- function() q_get("SELECT COALESCE(u.name,'-') AS name, COALESCE(u.email, u.phone) AS account, s.device, s.ip, s.page, s.started, s.last_seen FROM user_sessions s LEFT JOIN users u ON u.id = s.user_id WHERE s.ended IS NULL AND s.last_seen > ? ORDER BY s.last_seen DESC", list(ago_ts(120)))
admin_sessions <- function(n = 500) q_get("SELECT s.started, COALESCE(u.email, u.phone) AS account, s.device, s.ip, s.last_seen, s.ended FROM user_sessions s LEFT JOIN users u ON u.id = s.user_id ORDER BY s.started DESC LIMIT ?", list(n))
admin_login_events <- function(n = 1000) q_get("SELECT e.ts AS time, COALESCE(e.identifier, '-') AS identifier, e.method, CASE e.success WHEN 1 THEN 'Success' ELSE 'Failed' END AS result, e.reason, e.ip, e.device FROM login_events e ORDER BY e.id DESC LIMIT ?", list(n))
admin_runs <- function(n = 1000) q_get("SELECT r.id, COALESCE(u.email, u.phone) AS member, r.filename, r.n_rows AS rows, r.n_cols AS cols, r.target, r.score_before AS before, r.score_after AS after, r.grade, r.created_at FROM runs r LEFT JOIN users u ON u.id = r.user_id ORDER BY r.id DESC LIMIT ?", list(n))
admin_models <- function(n = 1000) q_get("SELECT m.created_at AS time, COALESCE(u.email, u.phone) AS member, m.filename, m.task, m.model, m.metric, ROUND(m.value, 4) AS value, ROUND(m.seconds, 2) AS seconds FROM model_runs m LEFT JOIN users u ON u.id = m.user_id ORDER BY m.id DESC LIMIT ?", list(n))
admin_audit <- function(n = 500) q_get("SELECT a.ts AS time, COALESCE(u.email, u.phone, '-') AS who, a.event, a.detail FROM audit a LEFT JOIN users u ON u.id = a.user_id ORDER BY a.id DESC LIMIT ?", list(n))
admin_login_trend <- function(days = 14) {
  d <- q_get("SELECT substr(ts,1,10) AS day, SUM(success) AS ok, SUM(1 - success) AS bad FROM login_events WHERE ts > ? GROUP BY substr(ts,1,10) ORDER BY day", list(ago_ts(days * 86400)))
  all <- format(Sys.Date() - (days - 1):0, "%Y-%m-%d"); m <- match(all, d$day)
  data.frame(day = all, ok = ifelse(is.na(m), 0, d$ok[m]), bad = ifelse(is.na(m), 0, d$bad[m]))
}
