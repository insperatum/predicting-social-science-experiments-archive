# This minimal script can be used to reproduce all of the main results from Fig 1
# You can change the settings below specify which model to evaluate, and which
# subset of experiments/effects to use for calculating its accuracy
# 
# With the default settings, the script will evaluate GPT-4 on all two-condition
# contrasts from all studies. It will output a table to results/results_minimal_example.csv
# with the following estimates:
# - The correlation between predicted and observed effects (≈ 0.83, adjusted ≈ 0.89)
# - The RMSE (≈ 11.1pp, adjusted ≈ 10.7pp)
# - The slope of under/over-estimation (≈ 0.57)
#
# Note that the analysis is stochastic, and so results may differ slightly
# Execution time: Approximately 2 minutes

library(tidyverse)
source("util.R")
source("config.R")


# ==== 1. Settings ==== ####
# Which model to evaluate?
# Options: human, babbage-002, davinci-002, gpt-3.5-turbo, gpt-4, deepseek/deepseek-chat-v3-0324, google/gemma-3-27b-it, openai/gpt-oss-120b
MODEL = "gpt-4"      

# Which demographic subgroup to use?
# Options: all, black, white, democrat, republican, male, female
PARTICIPANTS = "all"

# Which experimental contrasts to use?
# Options: FALSE (all possible pairs of conditions), TRUE (contrasts hypothesized by authors)
USE_HYPOTHESIZED_CONTRASTS = FALSE

# Which subset of studies/conditions/outcomes to use?
# Options: "all", "unpublished by 2021", "published by 2021", "published post 2021", "never published", "tess", "coppock/mullinix", "existing_attitudes", "sociology", "political_science", ""communication", "psychology", "social_policy" "unrecognized author" "small_effects", "large_effects"
FILTER_STUDIES = "all" 
# The subsets used in Figure 1 are defined below:
df_filters = tribble(
  ~spec, ~spec_filter,
  "all", \(x) x,
  "unpublished by 2021", \(x) filter(x, !study_published_by_2021),
  "published by 2021", \(x) filter(x, study_published_by_2021),
  "published post 2021", \(x) filter(x, study_published_after_2021),
  "never published", \(x) filter(x, study_never_published),
  "tess", \(x) filter(x, study_is_tess),
  "coppock/mullinix", \(x) filter(x, !study_is_tess),
  "existing_attitudes", \(x) filter(x, outcome_existing_attitude),
  "sociology", \(x) filter(x, study_is_sociology),
  "political_science", \(x) filter(x, study_is_political_science),
  "communication", \(x) filter(x, study_is_communication),
  "psychology", \(x) filter(x, study_is_psychology),
  "social_policy", \(x) filter(x, study_is_social_policy),
  "unrecognized author", \(x) filter(x, !study_recognize_author),
  "small_effects", function(x) {
    x %>% mutate(
      keep_idxs = map(df_reg, \(x) abs(x$estimate) < 0.05),
      df_reg = map2(df_reg, keep_idxs, \(x,y) x[y,]),
      df_llm = map2(df_llm, keep_idxs, \(x,y) x[y,]),
      V = map2(V, keep_idxs, \(x, y) as.matrix(x)[y,y])
    ) %>%
      filter(map_lgl(df_reg, \(x) nrow(x)>0))
  },
  "large_effects", function(x) {
    x %>% mutate(
      keep_idxs = map(df_reg, \(x) abs(x$estimate) >= 0.05),
      df_reg = map2(df_reg, keep_idxs, \(x,y) x[y,]),
      df_llm = map2(df_llm, keep_idxs, \(x,y) x[y,]),
      V = map2(V, keep_idxs, \(x, y) as.matrix(x)[y,y])
    ) %>%
      filter(map_lgl(df_reg, \(x) nrow(x)>0))
  }
)





# ==== 2. Load experiment/model data  ==== ####
# Load all experimental effects (df_experiments) and model-based predictions (df_predictions)
source("load_archive1_results.R")

# Filter predictions by model, demographic group, and whether the contrast is hypothesized by authors
d = df_experiments %>%
  inner_join(df_predictions %>% filter(model==MODEL)) %>%
  filter(participants==PARTICIPANTS) %>%
  filter(is.na(hypothesis) != USE_HYPOTHESIZED_CONTRASTS)

# Filter to a particular subset of studies / outcomes 
filt = filter(df_filters, spec==FILTER_STUDIES)$spec_filter[[1]]
d = d %>%
  left_join(study_features) %>%
  left_join(outcome_features) %>% 
  filt()






# ==== 3. Analysis ==== ####
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
    # Calculate the correlation between predicted and observed effects (see util.R)
    calc_correlation(df_est$df_reg.estimate, V, df_est$df_llm.prediction, return_ci = F) %>% rename(cor_adj=estimate),
    # Calculate the slope of under/over-estimation (see util.R)
    calc_metaregression(df_est$df_reg.estimate, V, df_est$df_llm.prediction),
    # Calculate the RMSE
    rmse_adj = sqrt(mean((df_est$df_llm.prediction - df_est$df_reg.estimate)^2 - diag(V))) * 100,
    rmse_raw = sqrt(mean((df_est$df_llm.prediction - df_est$df_reg.estimate)^2)) * 100
  )
})

# Take the median across all 32 runs
summarize(df_sims, across(c(cor_adj, cor_raw, rmse_adj, rmse_raw, b), median)) %>%
  write_csv("../results/results_minimal_example.csv")