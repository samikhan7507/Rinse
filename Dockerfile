# Rinse — Shiny for R, container image for Render / Docker / Fly.io.
#
# Why this base image: rocker/shiny already contains R, the Shiny package and
# httpuv (the native web-server dependency). The previous image (rocker/r-ver)
# shipped R with no packages, so a single failed install step produced the
# runtime error "there is no package called 'shiny'" on Render.
FROM rocker/shiny:4.3.3

ENV DEBIAN_FRONTEND=noninteractive

# System libraries the R packages compile/link against.
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    gfortran \
    libsodium-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libsqlite3-dev \
    libxml2-dev \
    libuv1-dev \
    zip \
    curl \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy the project first so install_packages.R has everything it needs.
COPY . /app

# Install every R package, then prove that the important ones actually load.
# A missing package now fails the *build* instead of killing the running app.
RUN mkdir -p storage output \
 && Rscript install_packages.R \
 && Rscript -e "pkgs <- c('shiny','DT','dplyr','readxl','writexl','DBI','RSQLite','jsonlite','httr','sodium','digest','zip','htmltools','randomForest','rpart','nnet','cluster'); bad <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]; if (length(bad)) stop('These packages do not load: ', paste(bad, collapse = ', ')); cat('All', length(pkgs), 'required R packages load correctly.\n')"

# Render mounts persistent disks as root, so the app runs as root inside the
# container (the container itself is not exposed beyond the platform proxy).
EXPOSE 3838
VOLUME ["/app/storage"]

# Render injects PORT; locally it falls back to 3838.
ENV PORT=3838

HEALTHCHECK --interval=30s --timeout=5s --start-period=60s --retries=3 \
  CMD curl -fsS "http://127.0.0.1:${PORT}/?health" || exit 1

CMD ["Rscript", "-e", "shiny::runApp(appDir = '/app', host = '0.0.0.0', port = as.integer(Sys.getenv('PORT', '3838')), launch.browser = FALSE)"]
