# demo_console.R - Rinse's R engine run from the console or RStudio, with no Shiny and no browser.
# It walks through Unit 2 (data types and structures) and Unit 3 (import, clean, dplyr, export)
# on a real file. Run it from the project root:
#     source("demo_console.R", echo = TRUE)

suppressPackageStartupMessages(library(dplyr))
for (f in c("config", "import", "profile", "score", "clean", "features"))
  source(file.path("R", paste0(f, ".R")), encoding = "UTF-8")

# ------------------------------------------------------------------ UNIT 2
cat("\n=== UNIT 2: vectors, lists, matrices, factors ===\n")
v <- c(a = 120, b = 45, c = 300, d = 87)      # a named numeric vector
print(v[2]); print(v[v > 100]); print(v * 2)  # accessing and arithmetic
v["b"] <- 50; print(v)                        # modifying
rinse <- list(name = APP_NAME, version = APP_VERSION, steps = c("Data", "Check", "Clean"))
print(rinse$steps[2]); print(rinse[["version"]])  # a list holds mixed types
m <- matrix(1:6, nrow = 2); print(m); print(t(m)); print(dim(m))
grade <- factor(c("low", "high", "low", "mid"), levels = c("low", "mid", "high"))
print(levels(grade)); print(table(grade))

# ------------------------------------------------------------------ UNIT 3: import
cat("\n=== UNIT 3: import ===\n")
df <- read_any(file.path("data", "samples", "loan_default.csv"))   # data frame, every column read as text
print(dim(df)); print(names(df)); str(df); print(head(df, 3))

demo <- df[1:50, ]                                                  # the same rows through other formats
f_csv <- tempfile(fileext = ".csv");  utils::write.csv(demo, f_csv, row.names = FALSE)
f_json <- tempfile(fileext = ".json"); jsonlite::write_json(demo, f_json)
f_xlsx <- tempfile(fileext = ".xlsx"); writexl::write_xlsx(demo, f_xlsx)
f_db <- tempfile(fileext = ".sqlite")
con <- DBI::dbConnect(RSQLite::SQLite(), f_db); DBI::dbWriteTable(con, "loans", demo); DBI::dbDisconnect(con)
cat("CSV:", dim(read_any(f_csv)), "| JSON:", dim(read_any(f_json)), "| Excel:", dim(read_any(f_xlsx)),
    "| SQLite:", dim(read_sqlite(f_db, "loans")), "\n")
# Web / API (needs internet):  web <- read_url("https://example.com/data.csv")   # CSV or JSON, https only

# ------------------------------------------------------------------ UNIT 3: missing data, filtering, subsetting
cat("\n=== UNIT 3: profile, score, clean ===\n")
target <- "defaulted"
prof <- profile_data(df, target)
print(prof$overview)
print(prof$columns[, c("name", "type", "missing_pct", "outlier_n")])
sc <- score_data(prof)
cat("Readiness score:", sc$score, "-", sc$grade, "\n"); print(head(sc$issues, 5))

res <- clean_data(df, DEFAULT_CLEAN, target, prof)
cleaned <- res$data
print(res$log)
cat("Rows", nrow(df), "->", nrow(cleaned), "| columns", ncol(df), "->", ncol(cleaned), "\n")
print(cleaned[1:3, 1:min(4, ncol(cleaned))])   # subsetting with [rows, columns]

# ------------------------------------------------------------------ UNIT 3: dplyr
cat("\n=== UNIT 3: dplyr select, filter, mutate, summarise, arrange ===\n")
num1 <- intersect(prof$columns$name[prof$columns$type == "numeric"], names(cleaned))[1]
cat("Working on the numeric column:", num1, "\n")
picked <- cleaned |> select(all_of(c(num1, target)))                                # select
print(head(picked, 3))
above  <- picked |> filter(.data[[num1]] > median(.data[[num1]]))                   # filter
cat("Rows above the median:", nrow(above), "of", nrow(picked), "\n")
flagged <- picked |> mutate(above_median = .data[[num1]] > median(.data[[num1]]))   # mutate
by_flag <- flagged |> group_by(above_median) |>
  summarise(rows = n(), average = mean(.data[[num1]]), .groups = "drop") |>         # summarise
  arrange(desc(rows))                                                               # arrange
print(by_flag)
by_target <- cleaned |> group_by(across(all_of(target))) |> summarise(rows = n(), .groups = "drop") |> arrange(desc(rows))
print(by_target)

# ------------------------------------------------------------------ UNIT 3: export
cat("\n=== UNIT 3: export ===\n")
out <- file.path(tempdir(), "rinse_demo"); dir.create(out, showWarnings = FALSE)
utils::write.csv(cleaned, file.path(out, "cleaned.csv"), row.names = FALSE)         # CSV
writexl::write_xlsx(cleaned, file.path(out, "cleaned.xlsx"))                        # Excel
con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "cleaned.sqlite"))          # database
DBI::dbWriteTable(con, "cleaned_data", cleaned, overwrite = TRUE)
print(DBI::dbGetQuery(con, "SELECT COUNT(*) AS rows_saved FROM cleaned_data")); DBI::dbDisconnect(con)
saveRDS(res$fit, file.path(out, "cleaning_rules.rds"))                              # R object
cat("Files written to", out, "\n"); print(list.files(out))
