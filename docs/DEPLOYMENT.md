# Deployment guide

## Checklist

| Item | How Rinse handles it |
|---|---|
| HTTPS | Terminated by Caddy (included) or nginx (example in `deploy/`). Shiny itself does not provide TLS. |
| Secrets | Environment variables only (`.env`, not committed). `.gitignore` excludes `.env`, databases and the signing secret. |
| Admin password | Set `RINSE_ADMIN_PASSWORD` before first start. The log warns while the default is in use. |
| Database safety | Parameterised queries, write-ahead logging, busy timeout, versioned migrations. |
| Health check | `GET /health` (or `/?health`) returns `{"status":"ok"}` (HTTP 200) or 503. Used by Render, Docker and Caddy. |
| Logging | `storage/logs/rinse.log`, rotated at 2 MB, last 60 lines visible in Admin > System. |
| Backups | `Rscript scripts/backup.R` or the Admin > System buttons. Schedule with cron (`scripts/backup.sh`). |
| Reproducible build | `Dockerfile` pins R 4.3.3. `docs/sessionInfo.txt` records the package versions the tests passed with. |
| Tests | `Rscript tests/run_tests.R`, also run by `.github/workflows/test.yml`. |
| Least privilege | Only the web port is published; the app writes to a single mounted `storage/` directory. The container runs as root inside the image because Render mounts persistent disks as root - if you host it yourself, add a `USER` line and `chown -R` of `/app/storage`. |

## Docker with automatic HTTPS

1. Point a DNS record at your server and open ports 80 and 443.
2. `cp .env.example .env` and set at least `RINSE_ADMIN_PASSWORD` and `RINSE_DOMAIN=rinse.yourdomain.com`.
3. `docker compose up -d`. Caddy requests a certificate on the first visit.
4. Open `https://rinse.yourdomain.com` and sign in as the administrator.

Set `GOOGLE_REDIRECT_URI=https://rinse.yourdomain.com` and add the same address in the Google Cloud Console.

## Render

Render has **no native R runtime** (its runtimes are Docker, Node, Python, Go, Ruby, Rust, Elixir and
static sites). A native-R service therefore never installs the R packages, and the app dies at start
with `there is no package called 'shiny'` - which is exactly the failure this section fixes. Rinse
ships a Docker service for Render instead.

**What the image does for you**

- `Dockerfile` starts from `rocker/shiny:4.3.3`, so R **and** Shiny (plus `httpuv`) are already there.
- `install_packages.R` installs every remaining package from Posit's pre-built binaries, falls back to
  CRAN source, then checks that each package actually **loads**. A missing package fails the build.
- The container listens on Render's `$PORT` (falling back to `3838`) and answers `/health`.

**Option A - import the blueprint (recommended)**

1. Push this repo to GitHub.
2. In Render: **New → Blueprint**, pick the repo. `render.yaml` creates a Docker web service with the
   right health check, disk and environment variables.
3. Open the service's **Environment** tab and set `RINSE_ADMIN_PASSWORD` (required before going public).
4. Deploy. Watch the log: the build prints `All 20 R packages are ready.` before the image is published.

**Option B - create the service by hand**

| Field | Value |
|---|---|
| Type | Web Service |
| Language / Runtime | **Docker** |
| Dockerfile path | `./Dockerfile` |
| Docker context | `.` |
| Health check path | `/health` |
| Disk | 1 GB, mounted at `/app/storage` |
| Environment | `RINSE_ADMIN_PASSWORD=...`, `RINSE_DB=storage/rinse.sqlite` (see below) |

`RINSE_DB` should be `storage/rinse.sqlite` - the path *inside* the container, which the disk keeps
across restarts and deploys.

**Already deployed as Sift or Vyrel?** Keep the service and its disk - only the names changed:

1. Optional: rename the service in Render's Settings (to `rinse`); the public URL follows.
2. Leave your existing `SIFT_*` or `VYREL_*` environment variables in place. Rinse reads them when the matching
   `RINSE_*` variable is absent, and it adopts `storage/sift.sqlite` or `storage/vyrel.sqlite` on first start.
3. Manual Deploy → **Clear build cache & deploy** so the new image is built.

**Option C - your existing Docker service**

Redeploy it. Nothing else is needed: the new `Dockerfile` installs the missing system libraries
(`libuv1-dev`, `libsqlite3-dev`, `libxml2-dev`, `gfortran`), pins the Shiny base image, and refuses to
publish an image that cannot load `shiny`.

> Free instances cannot mount a disk. Without one, accounts and history reset on every restart; with
> one, they survive. Choose the `starter` plan (already set in `render.yaml`) if you need persistence.

## Without Docker

Install R 4.1+, run `Rscript install_packages.R`, create `.Renviron` from `.env.example`, and run under a process manager such as systemd. Put nginx or Caddy in front for HTTPS (`deploy/nginx.conf.example`).

## Backups and restore

- Create: Admin > System > **Create backup now**, or `Rscript scripts/backup.R` (keeps 30 days).
- Restore: stop the app, replace `storage/rinse.sqlite` with a backup file, start the app.

## Monitoring

Point an uptime monitor at `https://your-domain/health`. Watch Admin > Overview for failed sign-ins: a sudden rise suggests password guessing. In Docker use `docker compose logs -f rinse`.

## Scaling notes

A single Rinse process serves one session at a time per R process, but Shiny handles many mostly idle sessions well. SQLite comfortably supports a few dozen active members. For larger teams run several containers behind a sticky-session load balancer and move the database to PostgreSQL by replacing `R/db.R`.
