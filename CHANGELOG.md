# Changelog

## 3.0.0 - renamed to Rinse
- **New name and logo.** Sift is now **Rinse**: a water drop with a check mark, in a flat teal tile. New favicon, wordmark and app icons (192, 512, maskable, Apple touch).
- **New look.** Plain white page, flat cards with thin borders, smaller corner radius and no gradients. The brand colour is deep teal, amber marks lost points and warnings, and the charts no longer use blue or violet. The left sidebar is unchanged.
- **Simpler sign-in.** Clearer wording and layout. "Continue with Google" appears only when Google sign-in is set up, and the Phone tab only when SMS is set up (or `RINSE_DEMO_SMS=1` for a classroom demo). Added a "forgot your password" hint. Browsers and password managers now recognise the email and password fields, so saved details fill in.
- **Settings renamed.** Environment variables are now `RINSE_*`. The old `SIFT_*` and `VYREL_*` names, the old database files (`sift.sqlite`, `vyrel.sqlite`) and models saved by earlier versions still work. Download and log files start with `rinse_`.
- Removed the old screenshots (they showed the previous name and colours).

## 2.2.0
- **dplyr in the engine.** `filter()` (dropping rows), `mutate()` (date parts), `arrange()` (sorting results, issues and drift), `group_by()` + `summarise()` (points lost per score category) and `select()` now appear in the cleaning, preparation, scoring and model code.
- **Real web-API import.** A link is treated as JSON when the server says so or the body starts with `{` or `[`, so API addresses that do not end in `.json` work. Nested JSON such as `{"data": [...]}` uses its first table.
- **Export to a database.** New "SQLite database" download: cleaned data, cleaning log and the train and test sets as tables in one `.sqlite` file.
- **`demo_console.R`.** The R engine run from the console: vectors, lists, matrices, factors, import from four formats, profile, score, clean, the five dplyr verbs and export.
- **Fixed:** the "Where points were lost" bars showed points kept; a date part that never changes (such as the year) is now left out and logged; k-nearest-neighbour filling no longer uses the target column.
- **Admin account from the environment.** No password is written in the code any more. `.env.example` holds the administrator email and password for local runs (`run.bat` and `run.sh` copy it to `.Renviron`) and for Render's "Add from .env". Without a password a random one is generated and printed once.
- Tests use the configured admin instead of hard-coded credentials; the end-to-end test also checks the SQLite download.
- `run.bat` quoting fixed; `.gitattributes` added; `.Renviron` and `.env` kept out of the Docker image; unused packages `class` and `MASS` removed; internal audit notes removed.
- Version 2.2.0.

## 2.1.3
- **Fixed a crash on startup.** The icon table lost an entry when the author-bar icons were deleted, which left a trailing comma in `ICON` and made R fail with "argument 11 is empty" before a single page could render. Sift now starts normally again.
- The test suite now **loads every module in the same order as `app.R`** and checks the UI dictionaries (icons, samples, admin tabs, steps, palette, models), so this class of typo fails loudly in CI instead of at startup.
- CI republishes the tail of a failed test log as an annotation, so the useful lines are visible next to the commit.
- Version 2.1.3.

## 2.1.2
- **The logo is now an S.** The mark was three stacked bars, which said nothing about the name. It is now a single bold, rounded "S" stroked in white on the same indigo-to-violet tile - a proper letter mark, legible down to 16 px in a browser tab.
- New logo, favicon, wordmark, app icons (192, 512, maskable, Apple touch) and refreshed screenshots all use the same geometry.
- Version 2.1.2.

## 2.1.1
- **Installable as an app (PWA)**: `manifest.json`, 192/512/maskable icons, an Apple touch icon, a passive service worker and a styled offline page. Chrome, Edge and Android now show the install icon in the address bar; iPhone/iPad get the Share → *Add to Home Screen* route.
- An **Install app** button appears in the sidebar (and in the phone menu) whenever the browser offers installation, with short instructions on iOS.
- The service worker never caches application data - it only answers page loads with the offline page when there is no network, so sign-in, uploads and live Shiny updates are unaffected.
- Version 2.1.1.

## 2.1.0
- **New name: Sift** (previously Vyrel) and a new mark - three bars funnelling down to an amber tip on an indigo-to-violet tile - so the logo finally says what the app does: sift the data, keep what is ready.
- **No author block in the app.** The bottom bar with the author name, GitHub, LinkedIn and Email links is gone. The interface now ends with the page content; a small version label sits at the bottom of the left panel, and Help stays in the navigation. Author links live in the README only.
- **Upload area rebuilt**: one large dashed drop zone with an upload icon ("Drop a file here, or click to browse"), the chosen file name, and a size note - instead of the stretched browser file box. The whole zone is clickable and accepts drag-and-drop.
- More room for content on phones: the fixed bottom navigation no longer overlaps cards, and the left panel keeps its compact user card.
- `SIFT_*` environment variables are the new names; the old `VYREL_*` names are still read as a fallback, and an existing `storage/vyrel.sqlite` is adopted automatically at startup.
- Filename-level rename only where it is cosmetic: log file, exports (`sift_report.xlsx` / `.html`), Docker volume and compose service names.

## 2.0.1
- **Brand**: new logo (indigo → violet rounded mark, white V, amber "ready" dot), new favicon, new wordmark, and a matching colour system - slate surfaces, indigo primary, amber accent, emerald/red for results. Every chart, badge, ring and button follows it.
- **Light theme only**: the dark theme, its toggle and the `Appearance` setting were removed. One calm light interface, no black backgrounds anywhere.
- **Footer rebuilt**: it now sits at the bottom of the page as a proper bar - brand block with version and tagline, icon pill links for GitHub, LinkedIn, Email and Help, and a privacy note - instead of three tiny links squeezed into the corner. Mobile gets a bottom navigation bar with safe-area padding.
- **Better on phones and laptops**: mobile-first spacing, 44 px touch targets, a stepper whose labels stay visible and scrolls sideways, single-column cards on narrow screens, sticky table headers, full-width buttons, and a sticky header with menu on phones.
- **Browser tab title** is just "Vyrel" (it used to show the whole tagline).
- **Sign-in screen** now lists what the app does, with tick icons, above the form.
- **Render deploy fixed**: Render has no native R runtime, so `render.yaml` now builds the Docker image. The `Dockerfile` uses `rocker/shiny:4.3.3` (Shiny and httpuv pre-installed), adds the missing system libraries (`libuv1-dev`, `libsqlite3-dev`, `libxml2-dev`, `gfortran`, …), runs `install_packages.R`, and **fails the build** if any package cannot load. `install_packages.R` tries Posit binaries, falls back to CRAN source, then verifies every package - so `there is no package called 'shiny'` can no longer reach production.
- Added `/health` (as well as `/?health`) for Render, Docker and uptime monitors.
- README rewritten: small real screenshots in a grid, proper feature list, working quick start and Render steps.
- Screenshots in `docs/screenshots/` refreshed to the new palette and trimmed to a readable size.

## 2.0.0
- New white, light interface with sidebar navigation (phone: bottom bar). Dark mode available from the sidebar and Settings.
- Machine learning: model comparison (logistic or linear, tree, random forest, k-NN), cross-validation, ROC, confusion matrix, feature importance, raw-versus-clean benchmark.
- Insights: correlation map, PCA, automatic k-means groups, anomaly detection.
- Predict page with a saved cleaning and encoding pipeline; model download and reload.
- Drift check with Population Stability Index.
- Privacy scan (emails, phones, national IDs, names) and target-leakage detection.
- k-nearest-neighbour imputation.
- Admin console: online now, sign-in history with device and IP, members, analyses, model runs, activity, database browser, system and backups. Searchable, filterable, exportable tables.
- Settings: profile, password change, own sign-in history, data export, account deletion.
- Operations: versioned migrations, WAL mode, structured logging, health endpoint, idle sign-out, Docker health check, docker-compose with Caddy HTTPS, backup scripts, CI.

## 1.0.0
- Import, profile, readiness score, cleaning, ML preparation and export.
- Email, phone and Google sign-in. Basic admin panel.
