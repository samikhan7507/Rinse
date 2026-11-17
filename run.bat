@echo off
cd /d "%~dp0"
if not exist .Renviron if exist .env.example copy .env.example .Renviron >nul
Rscript -e "shiny::runApp(host = '0.0.0.0', port = 3838, launch.browser = TRUE)"
pause
