#!/bin/sh
# Example cron entry (every night at 02:30):   30 2 * * * /path/to/rinse/scripts/backup.sh >> /path/to/rinse/storage/logs/backup.log 2>&1
cd "$(dirname "$0")/.." && Rscript scripts/backup.R
