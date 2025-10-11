## This script loads the data for Archive 1, and processes it to create predicted- and measured- effect sizes
# Creates:
# - df_experiments (rct estimates)
# - df_predictions (llm/forecaster predictions)
# - study_features (features of experiments at the study level)
# - outcome_features (features of experiments at the outcome level)

library(tidyverse)
library(progressr)
library(furrr)
library(scales)
source("util.R")

df_responses = read_rds("data/rct_responses.RDS")
df_hypotheses = read_rds("data/RA_hypotheses.RDS")
df_gpt_author_recognition = read_csv("data/gpt_author_recognition.csv")

# Study features
study_features = read_csv("data/RA_study_features.csv") %>%
  left_join(df_gpt_author_recognition %>% transmute(study, study_recognize_author = proportion_yes>0.5))
# Outcome features
outcome_features = read_csv("data/RA_outcome_features.csv")

# Rescale original data to [0,1]
d = df_responses %>%
  rowwise() %>%
  transmute(
    study,
    outcome.name,
    data = list(
      data %>% ungroup() %>% mutate(
        participant_id = as.character(row_number()),
        y = scales::rescale(as.numeric(y), to=c(0,1), from=c(outcome.min, outcome.max)),
      )
    )) %>%
  ungroup()

# Create more rows to include dataset for hypothesized contrasts
d = bind_rows(
  d,
  d %>% right_join(df_hypotheses) %>%
    rowwise() %>%
    mutate(data = list(data %>%
                         left_join(hypothesis_data, by="condition.name") %>%
                         filter(!is.na(t_hypothesis)) %>%
                         mutate(condition.name = paste0(hypothesis, "_", t_hypothesis)) %>% select(-t_hypothesis))) %>%
    select(-hypothesis_data)) %>%
  arrange(study, outcome.name)


# Note: Each row of d is a dataset to run a regression on, where y is the dependent variable)
run_regression = function(data, reference_condition, participants="all", pilot_frac=NA, subsample_seed=NA) {
  tryCatch({
    
    ## Filter to appropriate participants
    if(participants=="all")        df = data
    if(participants=="black")      df = data %>% filter(race_4=="Black")
    if(participants=="white")      df = data %>% filter(race_4=="White")
    if(participants=="male")       df = data %>% filter(GENDER=="Male")
    if(participants=="female")     df = data %>% filter(GENDER=="Female")
    if(participants=="democrat")   df = data %>% filter(pid_3=="Democrat")
    if(participants=="republican") df = data %>% filter(pid_3=="Republican")
    if(participants=="pilot")      df = {set.seed(subsample_seed); data %>% group_by(condition.name) %>% slice_sample(., prop=1) %>% slice_head(prop=pilot_frac) %>% ungroup()}
    if(participants=="target")     df = {set.seed(subsample_seed); data %>% group_by(condition.name) %>% slice_sample(., prop=1) %>% slice_tail(prop=1-pilot_frac) %>% ungroup()}
    
    if(!(reference_condition %in% df$condition.name) || (n_distinct(df$condition.name)<2)) return(tibble(err=list(T)))
    
    
    ## Run the regression
    fit = df %>%
      mutate(condition.name = fct_relevel(condition.name, reference_condition)) %>%
      lm(y ~ condition.name, .)
    
    
    ## Extract the regression coefficients and covariance matrix
    df_reg = broom::tidy(fit) %>%
      transmute(
        condition.name=str_match(term, "condition.name(.*)")[,2],
        estimate,
        std.error,
        p.value
      )
    i = !is.na(df_reg$condition.name)
    V = vcov(fit)[i,i]
    df_reg = df_reg[i,]
    
    tibble(df_reg = list(df_reg), V = list(V), N_regression=nrow(df))
    
  }, error=function(e) return(tibble(err=list(e))))
}



### Predictions
df_predictions = run_cached("output/processed_data/df_predictions.rds", \() with_progress({
  cat("Calculating predicted effects based on LLM responses... (takes a few minutes)")
  
  # Get forecaster predictions
  df_forecasting = read_rds("data/forecasting_responses.RDS") %>% mutate(value = value/100)
  
  # Get llm predictions
  df_llm = read_rds("data/llm_responses.RDS")
  df_llm_predictions = df_llm %>%
    mutate(expectation = ifelse(scale_flip, outcome_scale_max - (expectation-outcome_scale_min), expectation)) %>% # If original scale was unintuitive direction, LLM was prompted using a reverse scale. Flip result in these cases.
    mutate(expectation_rescaled = (expectation - outcome_scale_min)/(outcome_scale_max- outcome_scale_min)) # Rescale to [0,1]
  
  # Combine and add hypotheses
  df_model_predictions = bind_rows(
    df_llm_predictions %>% transmute(model, study, condition.name, outcome.name, y=expectation_rescaled, weight, spec_template_group),
    df_forecasting %>% transmute(model="human", study, condition.name, outcome.name, y=value, weight=1) %>% right_join(df_llm_predictions %>% select(study, outcome.name) %>% distinct, by=c("study", "outcome.name")) # 
  )
  df_model_predictions = bind_rows(
    df_model_predictions,
    df_model_predictions %>%
      inner_join(df_hypotheses %>% unnest(hypothesis_data), by=c("study", "condition.name", "outcome.name"), relationship="many-to-many") %>%
      filter(!is.na(t_hypothesis)) %>%
      mutate(condition.name = paste0(hypothesis, "_", t_hypothesis)) %>% select(-t_hypothesis)
  )
  
  # Calculate prediction condition means
  df_condition_means_model = tibble(participants=c("all", "black", "white", "male", "female", "democrat", "republican")) %>%
    pmap_dfr(function(participants) {
      df_model_predictions %>%
        group_by(model, study, outcome.name, hypothesis) %>%
        group_modify(function(d,k) {
          if(participants!="all") {
            if(k$model=="human") return(tibble()) else d = d %>% filter(spec_template_group==participants)
          }
          d = d %>% filter(!is.na(y))
          
          d %>%
            lm(y ~ 0 + condition.name, weights = weight, .) %>%
            broom::tidy() %>%
            transmute(participants, condition.name = str_match(term, "condition.name(.*)")[,2], estimate)
        }) %>%
        ungroup()
    })
  
  # Calculate contrasts from each condition to each reference condition
  df_predictions = df_condition_means_model %>%
    group_by(model, study, outcome.name, hypothesis, participants) %>%
    group_modify(function(d,k) {
      crossing(d, d %>% rename(reference_condition=condition.name, estimate.ref=estimate)) %>%
        filter(reference_condition != condition.name) %>%
        transmute(reference_condition, condition.name, prediction=estimate-estimate.ref)
    }) %>%
    ungroup() %>%
    nest(df_llm=c(condition.name, prediction))
  
  # Add human-llm combined prediction
  df_predictions = df_predictions %>% bind_rows(
    left_join(
      df_predictions %>% filter(model=="human", participants=="all") %>% transmute(study, outcome.name, hypothesis, participants, reference_condition, df_human=df_llm),
      df_predictions %>% filter(model=="gpt-4", participants=="all") %>% transmute(study, outcome.name, hypothesis, participants, reference_condition, df_gpt=df_llm)
    ) %>%
      unnest(df_human) %>% unnest(df_gpt, names_sep = "__") %>%
      filter(condition.name==df_gpt__condition.name) %>%
      mutate(prediction = (prediction + df_gpt__prediction)/2) %>%
      select(-df_gpt__condition.name, -df_gpt__prediction) %>%
      nest(df_llm=c(condition.name, prediction)) %>%
      mutate(model="combined")
  )
  
  df_predictions = df_predictions %>% inner_join(study_features %>% select(study))
  df_predictions
}))


df_experiments = run_cached("output/processed_data/df_experiments.rds", \() with_progress({
  p = progressor(steps = nrow(d))
  
  out = d %>% future_pmap_dfr(.options=furrr_options(seed = T), .f=function(study, outcome.name, hypothesis, data) {
    p()
    
    regressions = crossing(
      tibble(reference_condition = unique(data$condition.name)),
      tibble(participants=c("all", "black", "white", "male", "female", "democrat", "republican"))
    )
    
    regressions %>% pmap_dfr(function(...) {
      run_regression(data, ...) %>%
        mutate(study, outcome.name, hypothesis, ..., .before=0) %>%
        mutate(
          study_n = nrow(data),
          study_n_per_condition = nrow(data) / n_distinct(data$condition.name),
        )
    })
  })
  
  # Drop a couple of experiments which we excluded from prediction generation (Howard823 and Melin1066)
  out %>%
    right_join(df_predictions %>% select(study, outcome.name) %>% distinct, by=c("study", "outcome.name")) %>%
    inner_join(study_features %>% select(study))
}))



get_experiments_split = \() run_cached("output/processed_data/df_experiments_split.rds", \() with_progress({
  cat("Calculating estimated effects based on experimental data... (takes a few minutes)")
  p = progressor(steps = nrow(d))
  
  out = d %>% future_pmap_dfr(.options=furrr_options(seed = T), .f=function(study, outcome.name, hypothesis, data) {
    p()
    
    regressions = crossing(
      tibble(reference_condition = unique(data$condition.name)),
      crossing(
        tibble(participants=c("pilot", "target")),
        crossing(
          tibble(pilot_frac=seq(0, 1, 0.05)),
          tibble(subsample_seed=1:20)))
    )
    
    regressions %>% pmap_dfr(function(...) {
      run_regression(data, ...) %>%
        mutate(study, outcome.name, hypothesis, ..., .before=0) %>%
        mutate(
          study_n = nrow(data),
          study_n_per_condition = nrow(data) / n_distinct(data$condition.name),
        )
    })
  })
  
  # Drop a couple of experiments which we excluded from prediction generation (Howard823 and Melin1066)
  out %>%
    right_join(df_predictions %>% select(study, outcome.name) %>% distinct, by=c("study", "outcome.name")) %>%
    inner_join(study_features %>% select(study))
}))
