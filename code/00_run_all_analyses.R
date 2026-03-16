# Run all analyses, and output the time taken

dir.create("output", showWarnings = FALSE)
dir.create("output/processed_data", showWarnings = FALSE)

dir.create("../results", showWarnings = FALSE)
dir.create("../results/variables", showWarnings = FALSE)
dir.create("../results/plots", showWarnings = FALSE)

files = c(
  "0_minimal_example.R",
  "1_main_archive1.R",
  "2_main_archive2.R",
  "3_survey.R",
  "4_uses.R",
  "5_heterogeneity_archive1.R",
  "supplement_ensemble_size.R"
)

log = "../results/timings.csv"
cat("script,elapsed_sec,status\n", file = log)

for (f in files) {
  cat("\nRunning script: ", f, "\n")
  status <- "success"
  elapsed <- system.time(
    tryCatch(
      source(f),
      error = function(e) {
        status <<- "error"
        
        # Print error message
        message("Error: ", conditionMessage(e))
        
        # Print the call that caused the error
        if (!is.null(conditionCall(e))) {
          message("Call: ", deparse(conditionCall(e)))
        }
        
        # Print the stack trace
        message("\nTraceback:")
        calls <- sys.calls()
        # Remove the last few frames (tryCatch internals)
        calls <- head(calls, -2)
        for (i in rev(seq_along(calls))) {
          message(i, ": ", deparse(calls[[i]]))
        }
        
        NULL
      }
    )
  )["elapsed"]
  cat(sprintf("%s,%.3f,%s\n", f, elapsed, status), file = log, append = TRUE)
}
