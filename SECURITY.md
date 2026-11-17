# Security policy

## Reporting a problem

Please do not open a public issue for a security problem. Email **samikhan57705@gmail.com** with steps to reproduce. You will get a reply within a few days.

## What Rinse does

- Passwords are hashed with Argon2 (libsodium). Plain text is never stored or logged.
- Wrong passwords lock an account for 10 minutes after 5 attempts. One-time codes expire after 5 minutes and allow 5 tries.
- Every sign-in attempt is recorded with time, method, device and IP address.
- Session tokens and one-time codes are stored as hashes. Members are signed out after 30 idle minutes (configurable).
- All SQL uses bound parameters. The admin database browser is read-only, limited to known tables, and hides hash columns.
- Uploads are processed in memory, limited in size and rows. Link import accepts `https://` only and blocks private addresses.
- The Google sign-in `state` value is signed and expires after 10 minutes.

## What you must do in production

1. Change the default administrator password (`RINSE_ADMIN_PASSWORD`).
2. Serve over HTTPS (the compose file does this).
3. Do not set `RINSE_DEMO_SMS=1` in production. Connect an SMS provider, or leave phone sign-in off (the default).
4. Keep `.env` and `storage/` out of version control and back them up.
5. Review Admin > Sign-ins regularly.

## Known limits

- There is no email-based password reset. Administrators reset passwords.
- Rate limiting is per account, not per IP. Put a reverse proxy rate limit in front for public sites.
