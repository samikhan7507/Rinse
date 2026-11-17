#!/bin/sh
# Start Rinse on http://localhost:3838  (use run.bat on Windows)
# Set PORT to listen on a different port, e.g. PORT=8080 ./run.sh
cd "$(dirname "$0")" || exit 1
[ -f .Renviron ] || { [ -f .env.example ] && cp .env.example .Renviron; }
Rscript -e "shiny::runApp(host = '0.0.0.0', port = as.integer(Sys.getenv('PORT', '3838')))"
