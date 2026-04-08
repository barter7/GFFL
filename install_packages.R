# install_packages.R
# Run this script to install all required R packages before deploying or running locally.
# Usage: Rscript install_packages.R

packages <- c(
  "shiny",
  "bslib",
  "ffscrapr",
  "nflreadr",
  "dplyr",
  "tidyr",
  "ggplot2",
  "DT",
  "plotly",
  "purrr",
  "stringr",
  "scales"
)

# Optional packages (enhance visuals but not required)
optional_packages <- c("nflplotR")

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, repos = "https://cloud.r-project.org")
  } else {
    cat(pkg, "already installed\n")
  }
}

cat("=== Installing required packages ===\n")
for (pkg in packages) {
  install_if_missing(pkg)
}

cat("\n=== Installing optional packages ===\n")
for (pkg in optional_packages) {
  tryCatch(
    install_if_missing(pkg),
    error = function(e) cat("Skipping optional package", pkg, ":", e$message, "\n")
  )
}

cat("\nDone! You can now run the app with: shiny::runApp()\n")
