packages = c(
  # Core data manipulation and visualization
  "tidyverse",      # Includes dplyr, ggplot2, tidyr, readr, etc.

  # Parallel processing
  "furrr",          # Parallel processing with future and purrr
  "future",         # Parallel processing framework
  "future.callr",   # Future backend using callr
  "progressr",      # Progress reporting for long-running operations
  "parallel",       # Base R parallel computing

  # Advanced visualization
  "patchwork",      # Combining multiple plots
  "ggtext",         # Rich text in ggplot2
  "ggstance",       # Horizontal geoms for ggplot2
  "ggnewscale",     # Multiple color scales in ggplot2
  "scales",         # Scale functions for visualization

  # Statistical modeling
  "lme4",           # Linear mixed-effects models
  "metafor",        # Meta-analysis
  "broom",          # Tidying model outputs
  "fixest",         # Fast fixed-effects estimation

  # Data analysis utilities
  "corrr",          # Correlation analysis
  "apaTables",      # APA-style tables
  "Rmisc",          # Miscellaneous R functions (for summarySE)
  "Hmisc",          # Miscellaneous functions (for rcorr)
  "Matrix",         # Sparse and dense matrix classes
  "reshape",        # Data reshaping (for melt)
  "pracma",         # Optimization, used in metafor

  # Table formatting
  "knitr",          # Dynamic report generation
  "kableExtra"     # Advanced table formatting for knitr
)


cat("Checking and installing required packages...\n\n")

installed_packages = installed.packages()[, "Package"]
missing_packages = packages[!packages %in% installed_packages]

if (length(missing_packages) > 0) {
  cat("Installing missing packages:\n")
  cat(paste(" -", missing_packages, collapse = "\n"), "\n\n")

  for (pkg in missing_packages) {
    cat(paste("Installing", pkg, "...\n"))
    install.packages(pkg, repos = "https://cloud.r-project.org/", quiet = TRUE)
  }

  cat("\nAll packages installed successfully!\n")
} else {
  cat("All required packages are already installed.\n")
}

# Verify installation
still_missing = packages[!packages %in% installed.packages()[, "Package"]]
if (length(still_missing) > 0) {
  warning("Failed to install the following packages: ", paste(still_missing, collapse = ", "))
}

