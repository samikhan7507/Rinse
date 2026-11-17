<p align="center"><img src="docs/logo.svg" alt="Rinse" width="150"></p>

<p align="center"><b>Check, clean and prepare your data for machine learning.</b><br>
Score a messy dataset, clean it, train and compare models, and keep an eye on who is using the system.</p>

> Rinse used to be called *Sift*, and before that *Vyrel*. The old `SIFT_*` and `VYREL_*` environment variables and the old database files are still picked up automatically.

## Screenshots

Add your own screenshots to `docs/screenshots/` after the first run (sign-in, readiness score, cleaning, models, export) and link them here.

## What it does

Most of a data scientist's time goes into preparing data, and most student projects clean a file without ever measuring how good it is. Rinse measures first. The **ML-Readiness Score** turns "this data looks messy" into a number with itemised reasons. Then Rinse fixes what it safely can, trains real models on the result, and proves whether the cleaning helped.

| Messy loan file | After Rinse |
|---|---|
| Score 68. Income stored as `Rs. 20,506`, `BUSINESS` vs `Business`, `null` placeholders, duplicate rows, and a column that secretly leaks the answer | Score 98. Typed numbers, unified labels, leak removed, models trained: ROC AUC 0.82, macro F1 0.71 |

## Features

**Data readiness**
- Import CSV, TSV, Excel (sheet picker), JSON, SQLite (table picker) or a secure `https://` link, including web APIs that return JSON (nested JSON is flattened to its first table).
- Profile every column: types, missing and disguised missing values, duplicates, outliers, skew, IDs, constant columns, mixed-case labels, text numbers, mixed date formats.
- **Privacy scan:** finds emails, phone numbers, national IDs and names, and drops or masks them.
- **Target-leakage detection:** flags features that are almost a copy of the answer.
- **Readiness score** out of 100 in six categories, with a reason and a fix for every lost point.

**Cleaning and preparation**
- Every step optional, every action logged. Missing values filled by median, mean, most common value, or **k-nearest-neighbour machine learning**.
- Outlier capping, label unification, date parsing, duplicate and ID removal.
- Leak-free train and test split: scaling is learned from training rows only. One-hot or label encoding, stratified split.

**Machine learning**
- Trains and compares logistic or linear regression, decision tree, random forest and k-nearest neighbours.
- Accuracy, balanced accuracy, macro F1, ROC AUC, R squared, RMSE, MAE. Baseline comparison, k-fold cross-validation, ROC curves, confusion matrix, feature importance.
- **"Did cleaning help?"** trains the same model on the raw file and compares.
- **Insights:** correlation map, PCA, automatic k-means groups (best k by silhouette), anomaly detection.
- **Predict:** score a new file or a single record. Rinse applies the saved cleaning pipeline to new rows first. Download or reload the model as a file.
- **Drift check:** compares new data against the data a model learned from (Population Stability Index).

**Accounts and administration**
- Sign in with email and password, or Google or a mobile one-time code when those are set up. Argon2 password hashing, lockout, idle sign-out, remember-me.
- **Admin console:** live "online now" table, full sign-in history (time, method, device, IP, success or failure), members with disable, reset password, role change, sign-out-everywhere and delete, analyses, model runs, activity log, **read-only database browser**, system page, backups.
- Every table is searchable, sortable, filterable per column, and exports to CSV, Excel or the clipboard.
- Members can change their password, see their own recent sign-ins, download their data, and delete their account.

**Production readiness**
- Versioned database migrations, SQLite write-ahead logging, structured log file with rotation, `/health` endpoint, Docker image with health check, docker-compose with Caddy for automatic HTTPS and security headers, nightly backup script, CI workflow, 100+ automated checks.
- One plain, light interface on a white page (no dark mode): a deep teal brand colour with amber for warnings, real touch targets, and a layout that works from a 360 px phone to a wide laptop.

## Quick start

You need [R 4.1 or newer](https://cran.r-project.org/). RStudio is optional.

```bash
git clone https://github.com/samikhan7507/Rinse.git
cd Rinse
Rscript install_packages.R        # one time
cp .env.example .Renviron         # one time (Windows: copy .env.example .Renviron)
Rscript -e "shiny::runApp(port = 3838)"
```

Open <http://localhost:3838>. On Windows double-click `run.bat`. On macOS or Linux run `./run.sh`.

### First sign-in

The administrator account is created on the first start, with an empty database, from the settings in `.env.example`: `RINSE_ADMIN_EMAIL`, `RINSE_ADMIN_PASSWORD` and `RINSE_ADMIN_NAME`. `run.bat` and `run.sh` copy that file to `.Renviron` the first time you run Rinse, so the values apply automatically.

- That file is public on GitHub, so anyone can read the password in it. That is fine for a classroom demo on your own laptop. For a site other people can reach, set a different `RINSE_ADMIN_PASSWORD` in the host's environment before the first start, or change it in **Settings** after signing in.
- Changing the variables later does not change an account that already exists.
- If neither `.Renviron` nor `RINSE_ADMIN_PASSWORD` is present, Rinse generates a random password and prints it **once** in the start-up message.
- The password is stored only as an Argon2 hash. Anyone can create an ordinary member account on the **Sign up** tab, but only administrators see the Admin console.

### Try it in two minutes

On the first screen choose **Try a sample**. Six realistic datasets are built in. For the Predict and Drift pages, use `data/samples/loan_new_applicants.csv` (new applicants whose credit scores have dropped). See [docs/DEMO_GUIDE.md](docs/DEMO_GUIDE.md) for a presentation script and viva answers.

## Install it as an app (free, no store)

Rinse is a Progressive Web App, so it installs straight from the browser - on a laptop and on a phone,
with no app store, no developer account and no build step. Installed, it opens in its own window with
its own icon, and the browser chrome disappears.

| Where | How |
|---|---|
| Chrome / Edge on Windows, macOS or Linux | Click the **install icon** at the right end of the address bar (or menu → *Install Rinse*). The sidebar also shows an **Install app** button once the browser offers it. |
| Chrome on Android | Menu → **Install app** (or *Add to Home screen*). |
| Safari on iPhone / iPad | **Share** → **Add to Home Screen**. Safari has no automatic prompt, so use the button in the app. |
| Firefox | Install is not supported; use a bookmark. |

Requirements, all of which the app already meets: HTTPS (Render gives you that for free), a
`manifest.json` with 192 px and 512 px icons, and a small service worker (`www/sw.js`). The service
worker is deliberately passive - it only serves an "you are offline" page for page loads and never
caches your data, so sessions, uploads and live Shiny updates behave exactly as before.

Files: `www/manifest.json`, `www/sw.js`, `www/offline.html`, `www/icons/*.png` (192, 512, maskable,
Apple touch icon). Both are served from `www/`, so nothing else has to be configured.

## Turning on Google and phone sign-in

**Google.** Create an OAuth client ("Web application") in the [Google Cloud Console](https://console.cloud.google.com/apis/credentials), add your address as an authorised redirect URI, and set `GOOGLE_CLIENT_ID`, `GOOGLE_CLIENT_SECRET` and `GOOGLE_REDIRECT_URI` in `.Renviron`. Restart. The button becomes active.

**Phone.** Add a [Twilio](https://www.twilio.com/) SID, token and sender number as `TWILIO_ACCOUNT_SID`, `TWILIO_AUTH_TOKEN`, `TWILIO_FROM`. Without them the Phone tab is hidden. For a classroom demo set `RINSE_DEMO_SMS=1` to show it with the code on screen. That is handy for a demo but is not secure, so do not use it in production.

## Deploy

**Render (Docker).** Render has no native R runtime, so Rinse deploys as a Docker service - the included `render.yaml` blueprint and the `Dockerfile` do it for you. Import `render.yaml` at **New → Blueprint**, or create a Web Service with **Runtime: Docker**, Dockerfile path `./Dockerfile`, health check path `/health`, and a 1 GB disk mounted at `/app/storage`. In the Environment tab choose **Add from .env** and paste the lines from `.env.example` (or set `RINSE_ADMIN_PASSWORD` yourself). A persistent disk needs a paid Render plan; on a free plan the SQLite database is reset whenever the service restarts. The image installs and load-checks every R package at build time, so a missing package stops the build instead of crashing the running app. Full steps: [docs/DEPLOYMENT.md](docs/DEPLOYMENT.md#render).

**Docker with HTTPS (recommended for your own server).**

```bash
cp .env.example .env     # set RINSE_ADMIN_PASSWORD and RINSE_DOMAIN
docker compose up -d
```

Caddy gets a certificate for your domain automatically and adds security headers. Data lives in a Docker volume. Full guide, backups, monitoring and scaling notes: [docs/DEPLOYMENT.md](docs/DEPLOYMENT.md).

## Project structure

```
rinse/
├── app.R                  Entry point and health endpoint
├── demo_console.R         The R engine run from the console, no Shiny (Units II and III)
├── render.yaml            Render blueprint (Docker service)
├── R/
│   ├── config.R logger.R  Settings, structured logging
│   ├── db.R auth.R        SQLite migrations, sign-in, sessions, tracking, admin actions
│   ├── import.R           CSV, Excel, JSON, SQLite and URL import
│   ├── profile.R score.R  Profiling, privacy scan, leakage, readiness score
│   ├── clean.R features.R Cleaning engine, kNN imputation, encoding, scaling, split, saved pipeline
│   ├── ml.R               Models, CV, importance, raw-vs-clean, clustering, PCA, anomalies, drift
│   ├── export.R           Excel, HTML report, recipe, ZIP bundle
│   ├── ui_components.R    Interface pieces and SVG charts
│   ├── server.R           Shell and the seven-step workflow
│   ├── pages_ml.R         Models, insights, predict, drift screens
│   └── pages_admin.R      Admin console and settings
├── www/                   Logo, stylesheet, install manifest, service worker, client script
├── data/samples/          Six messy datasets, Excel, SQLite, new-applicants file
├── deploy/                Caddyfile, nginx example
├── scripts/               Backup scripts
├── tests/                 Unit tests and an end-to-end test
├── docs/                  Demo guide, architecture, deployment, screenshots
└── Dockerfile  docker-compose.yml  .env.example  install_packages.R
```

## Run the R engine in the console

The screens are a thin layer over plain R functions. To use them without Shiny, open R in the project folder and run `source("demo_console.R", echo = TRUE)`. It reads a sample file, builds vectors, lists, matrices and factors, imports from CSV, JSON, Excel and SQLite, profiles and cleans the data, uses the five dplyr verbs (`select`, `filter`, `mutate`, `summarise`, `arrange`) and exports to CSV, Excel, SQLite and RDS.

## Tests

```bash
Rscript tests/run_tests.R
```

Runs unit tests for the engine on all six samples, sign-in rules, lockout, OTP, tracking, admin actions, the database browser, every model type, drift and prediction. Then an end-to-end test drives the whole app from sign-in through training, prediction, drift and the admin console without a browser.

## Security notes

- Argon2 password hashes. One-time codes and session tokens stored only as hashes.
- Lockout for 10 minutes after 5 wrong passwords. OTP codes expire after 5 minutes with 5 tries.
- All database access uses parameterised queries. The admin database browser is read-only, restricted to known tables, and hides password hashes.
- Uploads are processed in memory, size and row limited. URL import allows `https://` only and blocks private addresses.
- Sign-ins record time, method, device and IP so you can spot abuse. Members can see their own sign-ins, export their data and delete their account.
- Put Rinse behind HTTPS in production (the compose file does this). See [SECURITY.md](SECURITY.md).

## Known limits

- The score is guidance about data quality. It does not predict model accuracy.
- Models are classical and run in R on one machine: up to 200,000 rows per file, and training uses at most 6,000 rows per model for speed.
- Predictions use the cleaning rules learned on the training data. If new data has a very different shape, run the Drift check first.
- SQLite suits one server and a few dozen concurrent users. For more, replace `R/db.R` with PostgreSQL.
- Cleaning values (medians, outlier limits, label spellings) are learned on the whole file before the split. Scaling and encoding use the training rows only.
- The six sample datasets are synthetic.
- Password reset is done by an administrator. There is no email-based reset because that needs a mail server.

## Author and team

Mini project for R Programming (Jaipur National University), built by **Md Sami Ahmad**, Mustafa, Hussain and Zaid.

**Md Sami Ahmad** · [GitHub](https://github.com/samikhan7507) · [LinkedIn](https://www.linkedin.com/in/samikhan07) · [samikhan57705@gmail.com](mailto:samikhan57705@gmail.com)

Released under the MIT License. See [CHANGELOG.md](CHANGELOG.md).
