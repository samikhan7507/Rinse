# Contributing

1. Fork, then create a branch.
2. `Rscript install_packages.R` and run `Rscript tests/run_tests.R`. All checks must pass before and after your change.
3. Keep one concern per file in `R/`. New behaviour needs a test in `tests/testthat/`.
4. Database changes go in a new entry in `MIGRATIONS` (`R/db.R`). Never edit an old entry.
5. Keep the interface flat and calm: use the CSS variables in `www/styles.css` (a single light theme - no dark mode), no gradients or glow, text at least 14 px, touch targets at least 44 px tall.
6. Open a pull request describing what changed and why.
