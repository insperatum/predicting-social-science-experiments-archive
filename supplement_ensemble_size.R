df_predictions_enemblesize = (with_progress({#run_cached("output/processed_data/df_predictions_ensemble.rds", \() with_progress({
  cat("Calculating predicted effects based on LLM responses... (takes a few minutes)")
  
  # Get forecaster predictions
  df_forecasting = read_rds("data/forecasting_responses.RDS") %>% mutate(value = value/100)
  
  # Get llm predictions
  df_llm = read_rds("data/llm_responses.RDS") %>% filter(model=="gpt-4")
  df_llm_predictions = df_llm %>%
    mutate(expectation = ifelse(scale_flip, outcome_scale_max - (expectation-outcome_scale_min), expectation)) %>% # If original scale was unintuitive direction, LLM was prompted using a reverse scale. Flip result in these cases.
    mutate(expectation_rescaled = (expectation - outcome_scale_min)/(outcome_scale_max- outcome_scale_min)) # Rescale to [0,1]
  
  df_model_predictions = df_llm_predictions %>% transmute(model, study, condition.name, outcome.name, y=expectation_rescaled, weight, spec_template_group, spec_group, hypothesis=NA)
  # Calculate prediction condition means
  participants="all"
  
  p = progressor(80)
  tibble(ensemble_size = c(1,2,4,8,15,30,60,120)) %>% crossing(repeat_idx=1:10) %>% pmap_dfr(function(ensemble_size, repeat_idx) {
    p()
    keep_spec_groups = df_llm_predictions %>% select(spec_group) %>% distinct %>% slice_sample(n=ensemble_size)
    df_condition_means_model = df_model_predictions %>%
      right_join(keep_spec_groups) %>%
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
    
    
    df_predictions = df_predictions %>% inner_join(study_features %>% select(study))
    df_predictions %>% mutate(ensemble_size, repeat_idx)
  })
}))


d = df_experiments %>%
  inner_join(df_predictions_enemblesize) %>%
  unnest(df_reg) %>%
  unnest(df_llm, names_sep="__") %>%
  filter(condition.name == df_llm__condition.name) %>% select(-df_llm__condition.name) %>% rename(prediction = df_llm__prediction) %>%
  filter(participants=="all", is.na(hypothesis), model=="gpt-4")


g = d %>%
  group_by(ensemble_size, repeat_idx) %>%
  group_modify(function(d,k) {
    tibble(r=cor(d$estimate, d$prediction))
  }) %>%
  group_by(ensemble_size) %>% summarize(r=mean(r)) %>%
  ggplot(aes(x=ensemble_size, y=r)) +
  geom_point() +
  geom_line() +
  geom_text(aes(label=str_glue("{ensemble_size} prompt{ifelse(ensemble_size>1, 's', '')}\nr = {sprintf('%.2f', r)}")),
            alpha=0.5,
            vjust=1.3,
            hjust=0,
            size=2.8
            ) +
  scale_x_log10(limits=c(0.8, 160)) +
  coord_cartesian(ylim=c(0.5,0.85)) +
  theme_bw() +
  labs(x="Number of GPT-4 prompts in ensemble", y="Raw correlation with measured effects")
ggsave("output/plots/ensemblesize.pdf", g, width=8, height=5)
