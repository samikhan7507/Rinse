# Creates a consistent copy of the database and removes copies older than 30 days.
# Run from the project root:  Rscript scripts/backup.R
for (f in c("config", "logger", "db", "auth")) source(file.path("R", paste0(f, ".R")))
p <- db_backup(); cat("Backup written:", p, "\n")
d <- dirname(p); old <- list.files(d, pattern = "\\.sqlite$", full.names = TRUE); old <- old[file.mtime(old) < Sys.time() - 30 * 86400]
if (length(old)) { file.remove(old); cat("Removed", length(old), "backups older than 30 days\n") }
