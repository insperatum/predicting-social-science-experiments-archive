library(tidyverse)
library(patchwork)
library(ggstance)
library(ggtext)
library(scales)
library(fixest)
source("util.R")
source("config.R")
source("load_archive1_results.R")

uses_variables = tibble()

# Get the experimental results for archive 1 when subsampling studies into "pilot" and "target"
# (Used for both pilots and replication analysis)
df_experiments_split = get_experiments_split() # Takes a few minutes
df_split = inner_join(
  df_experiments_split %>% filter(participants %in% c("pilot", "target")),
  df_predictions %>% filter(participants=="all", model=="gpt-4") %>% select(-participants)
) %>%
  filter(!map_lgl(df_reg, is.null)) %>%
  filter(map_dbl(df_reg, nrow) == map_dbl(df_llm, nrow)) %>% # Very rarely, pilot_frac is very small, effect cannot be esetimated for a particular condition
  unnest(c(df_llm, df_reg), names_sep=".") %>%
  transmute(study, subsample_seed, study_n_per_condition, outcome.name, hypothesis, reference_condition, condition.name=df_reg.condition.name, participants, pilot_frac,
            estimate=df_reg.estimate, std.error=df_reg.std.error, p.value=df_reg.p.value,
            pred=df_llm.prediction) %>%
  pivot_wider(id_cols = c(study, study_n_per_condition, outcome.name, hypothesis, reference_condition, pilot_frac, condition.name, subsample_seed, pred),
              names_from=participants, values_from=c(estimate, std.error, p.value))




#### Use-case: Conducting pilot studies (Figure 4A) ####
# This function is calibrates LLM-based predictions of effects against measured effects, based on a training set
# It fits a model on d_train, and then uses that model to predictions for every effect in d_test
get_pred_adj = function(d_train, d_test) {
  fit = d_train %>%  lm(estimate ~ pred, .)
  d_test %>% mutate(
    pred_adj = predict(fit, newdata=.),
    pred_adj_sigma = summary(fit)$sigma
  )
  
  ## Metafor alternative, produces essentially the same results ##
  # fit = d_train %>% rma(estimate~pred, sei=std.error, data=., method="REML")
  # d_test %>% mutate(
  #   pred_adj = predict(fit, newmods = d_test$pred)$pred,
  #   pred_adj_sigma = sqrt(fit$tau2) 
  # )
  
}

for(use_hypothesized in c(F, T)) {
  d_for_training = inner_join(
    df_experiments %>% filter(participants=="all", use_hypothesized != is.na(hypothesis)),
    df_predictions %>% filter(participants=="all", use_hypothesized != is.na(hypothesis), model=="gpt-4") %>% select(-participants)
  ) %>%
    unnest(c(df_llm, df_reg), names_sep=".") %>%
    transmute(study, n_per_condition=study_n_per_condition, outcome.name, hypothesis, reference_condition, condition.name=df_reg.condition.name, participants,
              estimate=df_reg.estimate, std.error=df_reg.std.error, pred=df_llm.prediction)
  
  sim_results = run_cached(str_glue("output/processed_data/pilot_sims_hypothesized={use_hypothesized}.rds"), \() with_progress({
    p = progressor(n_distinct(df_experiments$study))
    
    # Leave-one-study-out cross-validation
    unique(d_for_training$study) %>% future_map_dfr(function(test_study) {
      
      # Get train/test split
      d_train = d_for_training %>% filter(study!=test_study)
      d_test = df_split %>% filter(study==test_study, use_hypothesized != is.na(hypothesis))
      
      out = d_test %>%
        mutate(get_pred_adj(d_train, d_test)) %>% # Create LLM predictions for all test-study effects
        mutate(
          # Get weighted combination of pilot data and LLM 
          # Calculate optimal weight for LLM predictions and pilot estimates
          # based on the standard error of the pilot estimates, and the residual standard deviation of the LLM predisctions
          w_pred = std.error_pilot^2 / (std.error_pilot^2 + pred_adj_sigma^2), #
          pred_combined = case_when(
            pilot_frac==0 ~ pred_adj,
            T ~ w_pred*pred_adj + (1-w_pred)*estimate_pilot
          )
        ) %>%
        pivot_longer(c(estimate_pilot, pred_adj, pred_combined)) %>%
        mutate(err = estimate_target - value,
               mse_adj = err^2 - std.error_target^2,
               mean_N_train = mean(study_n_per_condition)*pilot_frac
        )
      
      p(); out
    })
  }))
  
  df_rmse = sim_results %>%
    filter(name != "pred_adj", pilot_frac <= 0.9) %>%
    group_by(study) %>%
      filter(!any(pilot_frac>0 & pilot_frac<1&is.na(mse_adj))) %>%
    group_by(study, pilot_frac) %>%
      filter(pilot_frac==0 | !any(is.na(mse_adj))) %>%
    group_by(pilot_frac, name) %>%
      mutate(n=mean(mean_N_train)) %>%
    group_by(name, n) %>%
      summarize(rmse_adj=sqrt(mean(mse_adj, na.rm=T))*100) %>%
    ungroup() %>%
    mutate(x=n*2, y=rmse_adj)
  
  
  df_interpolation = df_rmse %>%
    group_by(name) %>%
    group_modify(function(d, k) {
      f = approxfun(d$x, d$y, method = "linear")
      tibble(x = seq(0, max(d$x)), y = f(x))
    }) %>%
    ungroup()
  
  x1=100; y1=df_interpolation %>% filter(x==x1, name=="pred_combined") %>% with(y[[1]])
  y2=y1; x2=df_interpolation %>% filter(name=="estimate_pilot", y<y1) %>% with(min(x))
  x01=0; y01=df_interpolation %>% filter(x==x01, name=="pred_combined") %>% with(y[[1]])
  y02=y01; x02=df_interpolation %>% filter(name=="estimate_pilot", y<y01) %>% with(min(x))
  
  uses_variables = uses_variables %>% bind_rows(tribble(
    ~name, ~value,
    str_glue("uses_pilot_hypothesized{use_hypothesized}_N1_llm"), as.character(x01),
    str_glue("uses_pilot_hypothesized{use_hypothesized}_RMSE1_llm"), sprintf("%.1f", y01),
    str_glue("uses_pilot_hypothesized{use_hypothesized}_N2_llm"), as.character(x1),
    str_glue("uses_pilot_hypothesized{use_hypothesized}_RMSE2_llm"), sprintf("%.1f", y1),
    str_glue("uses_pilot_hypothesized{use_hypothesized}_N1_human"), as.character(x02),
    str_glue("uses_pilot_hypothesized{use_hypothesized}_RMSE1_human"), sprintf("%.1f", y02),
    str_glue("uses_pilot_hypothesized{use_hypothesized}_N2_human"), as.character(x2),
    str_glue("uses_pilot_hypothesized{use_hypothesized}_RMSE2_human"), sprintf("%.1f", y2)
  ))
  g = df_rmse %>%
    ggplot(aes(x=x, y=y,
               color=name %>% {c("estimate_pilot"="Human-only pilot", "pred_combined"="Human + LLM pilot")[.]} %>% fct_rev)) +
    scale_color_manual(values=c(hue_pal()(3)[[3]], "black")) +
    geom_point(alpha=0.5, size=1.3, stroke=0) +
    geom_line(alpha=0.5) +
    annotate("segment",
             x = x1+3, y = y1, xend = x2-3, yend = y2,
             linetype="dashed",
    ) +
    annotate("segment",
             x = x01+3, y = y01, xend = x02-3, yend = y02,
             linetype="dashed",
    ) +
    annotate("point", x = x1, y = y1) +
    annotate("point", x = x2, y = y2) +
    annotate("point", x = x01, y = y01) +
    annotate("point", x = x02, y = y02) +
    
    annotate("text", x = x01, y = y01, label=str_glue("LLM-only"), hjust=-0.1, vjust=2, size=2.7, alpha=0.7) +
    annotate("text", x = x02, y = y02, label=str_glue("N={x02}"), hjust=0.5, vjust=2, size=2.7, alpha=0.7) +
    annotate("text", x = x2, y = y2, label=str_glue("N={x2}"), hjust=0.5, vjust=2, size=2.7, alpha=0.7) +
    annotate("text", x = x1, y = y1, label=str_glue("N={x1} +LLM"), hjust=0.5, vjust=2, size=2.7, alpha=0.7) +
    theme_bw() + theme(
      legend.title = element_blank()
    ) +
    (
      if(use_hypothesized) coord_cartesian(xlim=c(0,700), ylim=c(0, 7), expand=F)
      else coord_cartesian(xlim=c(0, 300), ylim=c(0, 10), expand=F)
    ) +
    labs(x="N human participants", y="RMSE (adjusted)")
  plot(g)
  saveRDS(g, str_glue("output/processed_data/pilot_graph_hypothesized={use_hypothesized}.rds"))
}








#### Use-case: Forecasting replication success (Figure 4B) ####
df_split_replication = df_split %>%
  left_join(study_features) %>%
  mutate(
    across(c(pred, estimate_pilot, estimate_target), ~.*sign(estimate_pilot)),
    across(c(pred, estimate_pilot, estimate_target), ~.*100), # So that these are in "percentage points"
    replicate = p.value_target<0.05 & sign(estimate_target)==sign(estimate_pilot),
    z_pilot = estimate_pilot / std.error_pilot,
  )

m_base = df_split_replication %>% 
  filter(p.value_pilot<0.05) %>%
  filter(is.na(hypothesis), pilot_frac == .20) %>%
  # mutate(across(c(z_pilot, pred, study_n_per_condition, estimate_pilot), ~scale(.)[,1])) %>%
  feglm(replicate ~ z_pilot + pred + study_n_per_condition + estimate_pilot,
        family = "binomial",
        cluster = ~ study)

m_pub  = df_split_replication %>%
  filter(p.value_pilot<0.05) %>%
  filter(is.na(hypothesis), pilot_frac == .20, study_published_by_2021) %>%
  # mutate(across(c(z_pilot, pred, study_n_per_condition, estimate_pilot), ~scale(.)[,1])) %>%
  feglm(replicate ~ z_pilot + pred  + study_n_per_condition + estimate_pilot,
        family = "binomial",
        cluster = ~ study)

m_hyp  = df_split_replication %>% 
  filter(p.value_pilot<0.05) %>%
  filter(!is.na(hypothesis), pilot_frac == .20) %>%
  # mutate(across(c(z_pilot, pred, study_n_per_condition, estimate_pilot), ~scale(.)[,1])) %>%
  feglm(replicate ~ z_pilot + pred  + study_n_per_condition + estimate_pilot,
        family = "binomial",
        cluster = ~ study)

m_hyp_nocovariates  = df_split_replication %>% 
  filter(p.value_pilot<0.05) %>%
  filter(!is.na(hypothesis), pilot_frac == .20) %>%
  # mutate(across(c(z_pilot, pred, study_n_per_condition, estimate_pilot), ~scale(.)[,1])) %>%
  feglm(replicate ~ pred,
        family = "binomial",
        cluster = ~ study)


m_tf50 = df_split_replication %>%
  filter(p.value_pilot<0.05) %>%
  filter(is.na(hypothesis), pilot_frac == .50) %>%
  # mutate(across(c(z_pilot, pred, study_n_per_condition, estimate_pilot), ~scale(.)[,1])) %>%
  feglm(replicate ~ z_pilot + pred  + study_n_per_condition + estimate_pilot,
        family = "binomial",
        cluster = ~ study)

m_insig = df_split_replication %>%
  filter(p.value_pilot>0.05) %>%
  filter(is.na(hypothesis), pilot_frac == .20) %>%
  # mutate(across(c(z_pilot, pred, study_n_per_condition, estimate_pilot), ~scale(.)[,1])) %>%
  feglm(replicate ~ z_pilot + pred  + study_n_per_condition + estimate_pilot,
        family = "binomial",
        cluster = ~ study)

fileConn=file("../results/plots/replication_regressiontable.tex")
fixest::esttex(
  m_hyp_nocovariates, m_hyp, m_base, m_insig, m_tf50,  
  headers = list(
    ":_:Original subsample" = list("20%"=4,"50%"=1),
    ":_:Inclusion" = list(
      "Hypothesized\nsignificant"=2,
      "All\nsignificant"=1,
      "All\nnon-significant"=1,
      "All\nsignificant"=1
      )
  ),
  depvar = F,
  title    = "Logistic regression predicting effect replication, simulated by data subsamples",
  label    = "tab:robustness",
  dict     = c(z_pilot = "Original z-stat",
               estimate_pilot = "Original effect size",
               pred    = "LLM prediction",
               study_n_per_condition = "N per condition"
  ),
  order    = c("pred", "z_pilot", "study_n_per_condition"),
  signif.code = c("***"=0.001, "**"=0.01, "*"=0.05),
  fitstat=NA#c("pr2")
) %>% writeLines(fileConn)
close(fileConn)

uses_variables = uses_variables %>% bind_rows(
  tibble(
    model_name = c("hyp_no_covariates", "hyp", "base", "insig", "tf50"),
    fits = list(m_hyp_nocovariates, m_hyp, m_base, m_insig, m_tf50)
  ) %>%
    mutate(map_dfr(fits, function(x){broom::tidy(x) %>% filter(term=="pred") %>% select(estimate, p.value)})) %>%
    pivot_longer(c(estimate)) %>%
    transmute(
      name=str_glue("uses_replication_{model_name}_{name}"),
      value=sprintf("%.2f", value)
    )
)


uses_variables = uses_variables %>% bind_rows(
  tibble(
    model_name = c("hyp_no_covariates", "hyp", "base", "insig", "tf50"),
    fits = list(m_hyp_nocovariates, m_hyp, m_base, m_insig, m_tf50)
  ) %>%
    pmap_dfr(function(model_name, fits) {
      broom::tidy(fits) %>% select(term, estimate, p.value) %>%
        mutate(model_name) %>%
        mutate(odds = sprintf("%.1f", 100*(exp(estimate)-1)) %>% paste0("\\%"),
               estimate=sprintf("%.2f", estimate))
    }) %>%
    pivot_longer(c(estimate, odds)) %>%
    transmute(
      name=str_glue("uses_replication_{model_name}_{term}_{name}"),
      value
    )
)



for(use_hypothesized in c(F, T)) {
  x_vals = seq(min(df_split_replication$pred, na.rm=T), max(df_split_replication$pred, na.rm=T), 0.001)
  ptrain=0.2

  fit_0.05 = feglm(
    replicate ~ pred, family = binomial(), cluster=~study,
    data = df_split_replication %>% filter(use_hypothesized != is.na(hypothesis), p.value_pilot<0.05, pilot_frac==ptrain)
  )
  fit_0.01 = feglm(
    replicate ~ pred, family = binomial(), cluster=~study,
    data = df_split_replication %>% filter(use_hypothesized != is.na(hypothesis), p.value_pilot<0.01, pilot_frac==ptrain)
  )
  fit_0.001 = feglm(
    replicate ~ pred, family = binomial(), cluster=~study,
    data = df_split_replication %>% filter(use_hypothesized != is.na(hypothesis), p.value_pilot<0.001, pilot_frac==ptrain)
  )
  d_smooth = bind_rows(
    tibble(p=0.05, x = x_vals, y_smooth = predict(fit_0.05, newdata = tibble(pred = x_vals), type="response")),
    tibble(p=0.01, x = x_vals, y_smooth = predict(fit_0.01, newdata = tibble(pred = x_vals), type="response")),
    tibble(p=0.001, x = x_vals, y_smooth = predict(fit_0.001, newdata = tibble(pred = x_vals), type="response"))
  )
  
  g = d_smooth %>%
    mutate(significance = str_glue("p<{p}")) %>%
    ggplot(aes(x=x, y=y_smooth, color=significance)) +
    scale_color_manual(values=c("black", "gray50", "gray80")) +
    geom_line(aes(group=p)) +
    theme_bw() +
    coord_cartesian(xlim=c(-1.5,25), ylim=c(0.43,1), expand=F) +
    labs(x="LLM-predicted effect size (pp)", y="Replication probability") +
    ggtitle(str_glue("hypothesized: {use_hypothesized}"))
  plot(g)
  saveRDS(g, str_glue("output/processed_data/replication_graph_hypothesized={use_hypothesized}.rds"))
}







#### Use-case: Identifying effective interventions (Figure 4C) ####
df_studies_outcomes <- readRDS("../data/megastudies.RDS") %>%
  filter(dataset != "Dellavigna") %>% # Exclude this study because it's different outcomes for different treatments
  mutate(df = map(df, ~mutate(., condition.name=str_replace(condition.name, "–", "-")))) #annoying naming inconsistency :/
df_predictions_all = readRDS("../data/individual_expert_predictions.rds")

make_graph <- function(top_frac=1e-5) {
  get_w = function(pred) {
    # Weight assigned to each treatment, based on a set of predictions
    # (In this case, it's just uniform across the top treatments)
    w = rank(-pred, ties.method="min") <= ceiling(length(pred) * top_frac)
    w / sum(w)
  }
  
  # Loop over all studies
  # df has average effects and average predictions
  # df_predictions has forecaster predictions at the individual level
  df_results_by_study_outcome = inner_join(df_studies_outcomes, df_predictions_all) %>% pmap_dfr(function(dataset, outcome, df, V, df_predictions, ...) {
    # Some studies include conditions with no predictions. Remove these
    keep = df$condition.name %in% unique(df_predictions$condition.name)
    V = V[keep, keep]
    df = df[keep,]
    
    # Calculate the weight assigned to each treatment by each forecasting method
    # Note, we do this *at the forecaster level* and then average, because forecasters sometimes
    # only see a subset of treatments (and so GPT must choose the best _of those_)
    df_w = df_predictions %>%
      left_join(df) %>%
      group_by(forecaster_id) %>%
        filter(n()>1) %>% # If a forecaster only rated one condition, drop them
        mutate(
          w.forecaster = get_w(prediction),
          w.gpt = get_w(`prediction.gpt-4`),
          w.combined = get_w(percent_rank(prediction) + percent_rank(`prediction.gpt-4`))
        ) %>%
      group_by(condition.name) %>% # Now get the average weight across all forecasters
        summarize(across(c(w.forecaster, w.gpt, w.combined), mean))
    df = df %>% left_join(df_w)
    
    # Calculates the estimate and standard error for a weighted average of treatments
    f <- function(w) {tibble(
      estimate = sum(w * df$estimate.rct),
      se = as.numeric(sqrt(t(w) %*% V %*% w))
    )}

    f(df$w.combined - df$w.forecaster) %>%
      mutate(ATE_expert = f(df$w.forecaster)$estimate, dataset, outcome)
  })
  
  df_results_by_study = df_results_by_study_outcome %>%
    group_by(dataset) %>% # Average across outcomes
      summarize(across(c(estimate, se, ATE_expert), mean)) %>%
    ungroup() %>%
    mutate(dataset = ifelse(startsWith(dataset, "Broockman"), "Broockman", dataset)) %>%
    group_by(dataset) %>% # Average across independent sub-studies
      summarize(estimate = mean(estimate), se=sqrt(sum(se^2))/n(), across(c(ATE_expert), mean)) %>%
    ungroup() %>%
    mutate(across(c(estimate, se), ~./ATE_expert*100),
           conf.low=estimate-qnorm(0.975)*se, conf.high=estimate+qnorm(0.975)*se,
           across(c(estimate,conf.low,conf.high), ~.+100))
  
  # Calculate the meta-analytic average
  fit_metareg = rma.uni(yi=df_results_by_study$estimate, sei=df_results_by_study$se, method="REML")
  
  # Plot the results!
  df_graph = bind_rows(df_results_by_study, broom::tidy(fit_metareg, conf.int=T) %>% mutate(dataset="Average"))
  df_graph %>% 
    ggplot(aes(y=dataset %>% fct_reorder(estimate) %>% fct_relevel("Average"), x=estimate, xmin=conf.low, xmax=conf.high)) +
    geom_vline(xintercept=c(50, 200), color = "grey85", linewidth = 0.3) +
    geom_rect(data=tibble(x=1), inherit.aes=F,
              xmin=-Inf, xmax=Inf, ymin=0, ymax=1.55, color="gray", alpha=0.1) +
    geom_linerange(position=position_dodgev(0.4), alpha=0.8) +
    geom_point(position=position_dodgev(0.4)) +
    geom_text(aes(label=sprintf("%+.0f%%", estimate-100)), vjust=-0.6, alpha=0.8, size=2.8) + 
    theme_bw() + theme(
      panel.grid.major.y = element_blank()
    ) +
    scale_x_log10(breaks=c(50,100,200,400), labels=c("50%", "100%", "200%","400%"), ) +
    geom_vline(xintercept=100, linetype="dashed", alpha=0.5, color=hue_pal()(3)[[3]]) +
    coord_cartesian(xlim = c(40, 250)) +
    labs(y="Dataset", x="ATE of treatments chosen with vs. without AI (log-scale)")
}

g_top1 = make_graph()
plot(g_top1)
saveRDS(g_top1, file="output/processed_data/interventions_graph_top1.rds")
ggsave("../results/plots/interventions_graph_top1.pdf", g_top1, width=5, height=5)

g_top20 = make_graph(0.2)
plot(g_top20)
saveRDS(g_top20, file="output/processed_data/interventions_graph_top20.rds")




#### Save the variables ####

uses_variables %>% save_vars("uses.tex")


#### Combine all into a single figure (Fig 4) ####
for(use_hypothesized in c(F, T)) {

  pilot = readRDS(str_glue("output/processed_data/pilot_graph_hypothesized={use_hypothesized}.rds"))
  replication = readRDS(str_glue("output/processed_data/replication_graph_hypothesized={use_hypothesized}.rds"))
  interventions = readRDS("output/processed_data/interventions_graph_top20.rds")
  uses = readRDS("output/processed_data/survey_uses_graph.rds")
  
  spc = plot_spacer()
  
  t = theme(
    plot.title = element_text(face="bold"),
    legend.position = "none",
    axis.title.y = element_text(face="bold")
  )
  g1 = pilot + t + ggtitle("A. Use-case: Conducting pilot studies") + labs(x="Number of human participants") +
    annotate(
      geom="text",
      x=if(use_hypothesized) 550 else 250,
      y=if(use_hypothesized) 3.5 else 4.5,
      angle=if(use_hypothesized) -6 else -5,
      hjust=0.5,
      label="Human-only pilot",
      size=3.4,
      color=hue_pal()(3)[[3]],
      alpha=0.8,
      fontface="bold"
    ) +
    annotate(
      geom="text",
      x=if(use_hypothesized) 550 else 250,
      y=if(use_hypothesized) 2.1 else 2.9,
      angle=if(use_hypothesized) -3 else -5,
      hjust=0.5,
      label="Human + LLM pilot",
      size=3.4,
      # color=hue_pal()(3)[[3]],
      alpha=0.8,
      fontface="bold"
    )
  g2 = replication + t + ggtitle("B. Use-case: Forecasting replication success") +
    annotate(
      geom="text",
      x=0.03,
      y=if(use_hypothesized) 0.955 else 0.87,
      label="p < 0.001",
      size=3, alpha=0.85,
      angle=if(use_hypothesized) 8 else 10,
      fontface="bold"
    ) +
    annotate(
      geom="text",
      x=0.03,
      y=if(use_hypothesized) 0.87 else 0.72,
      label="p < 0.01",
      size=3, alpha=0.6,
      angle=if(use_hypothesized) 30 else 30,
      fontface="bold"
    ) +
    annotate(
      geom="text",
      x=0.03,
      y=if(use_hypothesized) 0.66 else 0.53,
      label="p < 0.05",
      size=3, alpha=0.3,
      angle=if(use_hypothesized) 50 else 35,
      fontface="bold"
    ) +
    labs(x="LLM-predicted effect size (pp)", y="Replication success probability")

  g3 = interventions + t +ggtitle("C. Use-case: Identifying effective interventions") + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    geom_richtext(aes(
      label=dataset %>%
        str_replace_all(
          c("Milkman2021"="Milkman et al. (2021)",
            "Saccardo2024"="Saccardo et al. (2024)",
            "Goldwert"="Goldwert et al. (unpublished)",
            "Broockman"="Broockman et al. (2024)",
            "Doell"="Vlasceanu et al. (2024)",
            "DellavignaPope"="Dellavigna and Pope (2018)",
            "Mason"="Mason et al. (unpublished)",
            "Milkman2022"="Milkman et al. (2022)",
            "SDC"="Voelkel et al. (2024)",
            "Zickfeld"="Zickfeld et al. (2025)",
            "Milkman2023"="Milkman et al. (2025)"
          )) %>%
        str_replace("Average", "<b>Average</b>") %>%
        fct_reorder(estimate) %>%
        fct_relevel("<b>Average</b>"),
      alpha = ifelse(str_detect(dataset, "Average"), 1, 0.5),
      # x=29.5
      x=42
      # x=conf.low-1
    ),
    hjust = 0,
    # hjust=1,
    # vjust=0,
    size=2.8,
    # alpha=0.8,
    fill=NA,
    label.color=NA
    ) +
    scale_alpha_identity() +
    # coord_cartesian(ylim=c(0.5, 13.1), xlim=c(29, 210), expand=F) +
    coord_cartesian(ylim=c(0.5, 12.6), xlim=c(41, 210), expand=F) +
    labs(x="Treatment effect of interventions selected by combining expert judgment with LLM") +
    # annotate(
    #   "segment", x=92, y=12.65, xend=98, yend=12.65, arrow=arrow(type = "closed", length = unit(3, "pt")),
    #   alpha=0.3,
    #   color=hue_pal()(3)[[3]]
    # ) +
    annotate(
      # "text", x=91, y=12.65, label="Expert-only selection", hjust = 1, alpha=0.7,
      "text", x=100, y=8.5, label="Expert-only selection", hjust = 0.5, vjust=-0.5, alpha=0.8, angle=90,
      
      size=3.4,
      fontface="bold",
      color=hue_pal()(3)[[3]]
    ) +
    theme(
      panel.grid.major.x = element_blank()
    )

  g4 = uses + t + ggtitle("D. Survey of social scientists: Likelihood of use") +
    scale_x_continuous(expand=c(0,0), breaks=c(0, 25, 50, 75), labels=c("0%", "25%", "50%", "75%")) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_text(angle=90), panel.border=element_blank(), axis.line.x = element_line()) +
    labs(y="Use-case", x="Likelihood of use (0-100)") +
    geom_richtext(
      aes(label=label, x=0,
          color=ifelse(str_detect(label, "Overall"), "white", "black")),
      hjust=0,
      size=2.8, alpha=1,
      fill=NA, label.color=NA
    ) +
    scale_color_identity() +
    coord_cartesian(xlim=c(0,75))
  
  g = wrap_plots(g1, spc, g2,
                 spc, spc, spc,
                 free(g3, type="label", side="l"), spc, free(g4, side="l"),
                 ncol=3, widths=c(1,.02,1.05), heights=c(1,.02,1.2))
  plot(g)
  ggsave(str_glue("../results/plots/uses_megaplot_hypothesized={use_hypothesized}.pdf"), g, width=12, height=9)
}



