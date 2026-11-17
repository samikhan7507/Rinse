Sys.setenv(RINSE_DB = tempfile(fileext = ".sqlite"))
for (f in list.files("../../R", pattern = "(config|logger|db|auth)\\.R$", full.names = TRUE)) source(f)
db_init()

test_that("admin is seeded and can sign in; password is hashed", {
  r <- login_email(CFG$admin_email, CFG$admin_password); expect_true(r$ok); expect_equal(r$user$role, "admin")
  h <- q_get("SELECT pw_hash FROM users WHERE role='admin'")$pw_hash; expect_false(grepl(CFG$admin_password, h, fixed = TRUE))
  expect_false(login_email(CFG$admin_email, "wrong")$ok)
})
test_that("registration rules", {
  expect_false(register_user("A", "bad", "abcd1234")$ok)
  expect_false(register_user("A", "a@b.com", "short")$ok)
  expect_false(register_user("A", "a@b.com", "onlyletters")$ok)
  expect_true(register_user("Asha", "asha@example.com", "Strong123")$ok)
  expect_false(register_user("Asha", "ASHA@example.com", "Strong123")$ok)
  expect_true(login_email("Asha@Example.com", "Strong123")$ok)
})
test_that("lockout after repeated failures", {
  register_user("Lock", "lock@example.com", "Strong123")
  for (i in 1:5) login_email("lock@example.com", "nope")
  r <- login_email("lock@example.com", "Strong123"); expect_false(r$ok); expect_match(r$msg, "Too many")
})
test_that("phone OTP flow", {
  expect_false(send_otp("123")$ok)
  s <- send_otp("98765 43210"); expect_true(s$ok); expect_equal(s$phone, "+919876543210"); expect_true(s$demo)
  expect_false(verify_otp(s$phone, "000000")$ok)
  v <- verify_otp(s$phone, s$code); expect_true(v$ok); expect_equal(v$user$provider, "phone")
  expect_false(verify_otp(s$phone, s$code)$ok)   # code is single use
})
test_that("admin management", {
  u <- admin_users(); expect_gte(nrow(u), 3)
  id <- u$id[which(u$email == "asha@example.com")]; admin_set_active(id, FALSE, 1)
  expect_false(login_email("asha@example.com", "Strong123")$ok)
  admin_delete_user(u$id[which(u$role == "admin")], 1); expect_equal(sum(admin_users()$role == "admin"), 1)
  expect_gt(nrow(admin_audit()), 0); expect_true(is.list(admin_stats()))
})

test_that("sessions and oauth state", {
  tok <- create_session(1); expect_equal(session_user(tok)$id, 1); end_session(tok); expect_null(session_user(tok)); expect_null(session_user("bad"))
  st <- make_state(); expect_true(check_state(st)); expect_false(check_state("123-abc")); expect_false(check_state(paste0(st, "x")))
})

test_that("sign-ins are tracked with device and ip", {
  ctx <- list(ip = "10.1.2.3", ua = "Mozilla/5.0 (Linux; Android 13) AppleWebKit Chrome/120 Mobile Safari/537.36", device = parse_device("Mozilla/5.0 (Linux; Android 13) Chrome/120 Mobile Safari/537.36"))
  expect_match(ctx$device, "Mobile / Android / Chrome")
  login_email(CFG$admin_email, "bad-pass", ctx); login_email(CFG$admin_email, CFG$admin_password, ctx)
  ev <- admin_login_events(); expect_true(all(c("Success", "Failed") %in% ev$result)); expect_true("10.1.2.3" %in% ev$ip)
  uid <- q_get("SELECT id FROM users WHERE email = ?", list(CFG$admin_email))$id
  sid <- track_start(uid, ctx); expect_gte(nrow(admin_online()), 1); expect_gte(admin_stats()$online, 1)
  track_beat(sid, "history"); track_end(sid); expect_equal(nrow(admin_online()), 0)
  expect_equal(nrow(admin_login_trend(7)), 7)
})
test_that("password change, reset, role, export, delete", {
  r <- register_user("Rita", "rita@example.com", "Strong123"); id <- r$user$id
  expect_false(change_password(id, "wrong", "Newpass123")$ok); expect_true(change_password(id, "Strong123", "Newpass123")$ok)
  expect_true(login_email("rita@example.com", "Newpass123")$ok)
  tmp <- admin_reset_password(id, 1); expect_true(login_email("rita@example.com", tmp)$user$must_change)
  expect_true(change_password(id, tmp, "Another456")$ok); expect_true(admin_set_role(id, "admin", 1)); expect_equal(get_user(id)$role, "admin")
  admin_set_role(id, "user", 1); ex <- export_user_data(id); expect_equal(ex$profile$email, "rita@example.com")
  expect_true(delete_account(id)$ok); expect_null(get_user(id))
  expect_false(delete_account(q_get("SELECT id FROM users WHERE role='admin'")$id[1])$ok)
})
test_that("database browser hides secrets; backup and migrations work", {
  expect_true("login_events" %in% db_tables()); expect_false("pw_hash" %in% names(db_read_table("users")))
  expect_error(db_read_table("sqlite_master; DROP TABLE users")); st <- db_stats(); expect_gte(st$version, 2)
  b <- db_backup(tempfile(fileext = ".sqlite")); expect_true(file.exists(b)); expect_true(db_healthy())
})
