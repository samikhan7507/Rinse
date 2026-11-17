# Architecture

```
Browser (phone or laptop)  ──HTTPS──▶  Caddy (certificates, headers)  ──▶  Shiny app (R/)
                                                                              │
                         ┌────────────────────────────────────────────────────┤
                         ▼                                                    ▼
          SQLite (WAL, migrations)                                 Pipeline (in memory)
   users · login_events · user_sessions                import → profile → score → clean
   runs · model_runs · audit · sessions                → prepare → models → insights → export
```

## Data flow for one analysis

1. `import.R` reads everything as text so no value is silently changed on the way in.
2. `profile.R` infers each column's type and measures problems, personal data and target leakage.
3. `score.R` turns measurements into deductions with a reason and a fix.
4. `clean.R` applies the chosen fixes in a fixed order and logs each one: placeholders to blanks, privacy and leakage drops, numbers and dates parsed, labels unified, duplicates removed, columns dropped, outliers capped, then gaps filled (so the median is not distorted by outliers). It also stores what it learned (`fit`).
5. `features.R` splits first, encodes, then scales using training statistics, and stores the encoders (`spec`).
6. `ml.R` trains models on the training rows and judges them on the test rows. It also builds the **bundle**: model + `fit` + `spec`, which is everything needed to score raw rows later.
7. `export.R` builds the files.

## Sign-in and tracking

`auth.R` handles three sign-in methods. Each attempt writes a `login_events` row (time, method, success, reason, IP, device). Each browser session writes a `user_sessions` row that is refreshed every 30 seconds, so "online now" means "seen in the last two minutes". The admin console only reads these tables. The database browser lists tables from a fixed allow-list and hides hash columns.

## Design decisions

- **One file per concern**, so each stage is testable without the interface.
- **Everything is logged**: cleaning is explainable and repeatable (`recipe.json`), administrator actions go to the audit table, operations go to `storage/logs/rinse.log`.
- **SQLite with migrations**: zero setup, safe upgrades (`MIGRATIONS` in `db.R`). Only `q_get` and `q_exec` touch the database, so moving to PostgreSQL is a contained change.
- **Models without heavy dependencies**: rpart, randomForest, class, nnet, cluster and base `stats`.
- **No UI framework theme**: one light stylesheet with CSS variables (no dark mode), charts are inline SVG, so there is nothing to break offline.
- **Sessions** use a random token in the browser; only its hash is stored.
