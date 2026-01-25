#
# Execution time: Approximately 1 minute
#
# This minimal script reproduces the main result of the paper: the accuracy o
# GPT-4 predictions across all 70 studies in the primary archive. It calculates:
# - The correlation between predicted and observed effects (≈ 0.83, adjusted ≈ 0.89)
# - The RMSE (≈ 11.1pp, adjusted ≈ 10.7pp)
# - The slope of under/over-estimation (≈ 0.57)
#
# Note that the analysis is stochastic, and so results may differ slightly
#

library(tidyverse)
source("util.R")
source("config.R")

# First load and filter the experimental data and corresponding GPT-4 predictions
# We filter with:
#   participants=="all", for effects measured across all participants (rather than e.g. only female participants)
#   is.na(hypothesis), to measure accuracy on random experimental contrasts (rather than contrasts hypothesized by authors)
source("load_archive1_results.R")
d = inner_join(df_experiments, df_predictions) %>%
  filter(model=="gpt-4", participants=="all", is.na(hypothesis))

# Our analysis is stochastic, and so we repeat it 32 times. This is because, for
# each study, we randomly select one outcome variable and one reference condition.
df_sims = 1:n_runs %>% map_dfr(function(run_idx) {
  cat(str_glue("Run {run_idx} of {n_runs}\n\n"))
   
  # Subsample the data (Select a random outcome & reference condition per study)
  dd = d %>%
    group_by(study) %>%
      filter(reference_condition == sample(reference_condition, 1)) %>%
      filter(outcome.name == sample(outcome.name, 1)) %>%
    ungroup() %>%
    select(study, reference_condition, outcome.name) %>%
    distinct %>%
    right_join(d, ., by=c("study", "outcome.name", "reference_condition"))
  
   # Create a dataframe with one row per effect, and the corresponding covariance matrix across effects
   df_est = dd %>% unnest(c(df_reg, df_llm), names_sep=".")
   V = bdiag(dd$V)
    
   # Run the analysis
   mutate(
     # Calculate the correlation between predicted and observed effects
     calc_correlation(df_est$df_reg.estimate, V, df_est$df_llm.prediction, return_ci = F) %>% rename(cor_adj=estimate),
     # Calculate the slope of under/over-estimation
     calc_metaregression(df_est$df_reg.estimate, V, df_est$df_llm.prediction),
     # Calculate the RMSE
     rmse_adj = sqrt(mean((df_est$df_llm.prediction - df_est$df_reg.estimate)^2 - diag(V))) * 100,
     rmse_raw = sqrt(mean((df_est$df_llm.prediction - df_est$df_reg.estimate)^2)) * 100
   )
})

# Take the median across all 32 runs
summarize(df_sims, across(c(cor_adj, cor_raw, rmse_adj, rmse_raw, b), median))
