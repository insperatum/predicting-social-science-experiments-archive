library(tidyverse)
library(Matrix)
library(metafor)
library(broom)
library(ggstance)
library(ggtext)
source("config.R")
source("util.R")
source("load_archive1_results.R")

# Fig 5. Subgroups analysis ####
d_subgroups = df_experiments %>%
  left_join(study_features) %>%
  left_join(outcome_features) %>%
  inner_join(df_predictions) %>%
  rowwise %>% filter(!is.null(df_reg), !is.null(df_llm)) %>% ungroup() %>% # Removes 
  filter(participants != "all", model == "gpt-4")

d_ethnicity = inner_join(
  d_subgroups %>% filter(participants=="white") %>% select(study, reference_condition, outcome.name, hypothesis, df_llm),
  d_subgroups %>% filter(participants=="black") %>% select(study, reference_condition, outcome.name, hypothesis, df_llm),
  by=c("study", "reference_condition", "outcome.name", "hypothesis")
) %>%
  filter(map2_lgl(df_llm.x, df_llm.y, function(x,y) nrow(x)==nrow(y) && all(x$condition.name == y$condition.name))) %>%
  select(study, reference_condition, outcome.name, hypothesis)
d_gender = inner_join(
  d_subgroups %>% filter(participants=="female") %>% select(study, reference_condition, outcome.name, hypothesis, df_llm),
  d_subgroups %>% filter(participants=="male") %>% select(study, reference_condition, outcome.name, hypothesis, df_llm),
  by=c("study", "reference_condition", "outcome.name", "hypothesis")
) %>%
  filter(map2_lgl(df_llm.x, df_llm.y, function(x,y) nrow(x)==nrow(y) && all(x$condition.name == y$condition.name))) %>%
  select(study, reference_condition, outcome.name, hypothesis)
d_party = inner_join(
  d_subgroups %>% filter(participants=="democrat") %>% select(study, reference_condition, outcome.name, hypothesis, df_llm),
  d_subgroups %>% filter(participants=="republican") %>% select(study, reference_condition, outcome.name, hypothesis, df_llm),
  by=c("study", "reference_condition", "outcome.name", "hypothesis")
) %>%
  filter(map2_lgl(df_llm.x, df_llm.y, function(x,y) nrow(x)==nrow(y) && all(x$condition.name == y$condition.name))) %>%
  select(study, reference_condition, outcome.name, hypothesis)

specs_subgroups = tribble(
  ~spec, ~spec_filter,
  "black", \(x) filter(inner_join(x, d_ethnicity), participants=="black", is.na(hypothesis)),
  "white", \(x) filter(inner_join(x, d_ethnicity), participants=="white", is.na(hypothesis)),
  "female", \(x) filter(inner_join(x, d_gender), participants=="female", is.na(hypothesis)),
  "male", \(x) filter(inner_join(x, d_gender), participants=="male", is.na(hypothesis)),
  "democrat", \(x) filter(inner_join(x, d_party), participants=="democrat", is.na(hypothesis)),
  "republican", \(x) filter(inner_join(x, d_party), participants=="republican", is.na(hypothesis)),
)
spec_comparisons = tribble(
  ~spec_group, ~spec1, ~spec2, ~extra_globals,
  "Ethnicity", "white", "black", list(d_ethnicity),
  "Party", "republican", "democrat", list(d_party),
  "Gender", "male", "female", list(d_gender)
)


#### Accuracy by subgroup ####
sims_subgroups = run_sims(d_subgroups, specs_subgroups)

subgroup_N = specs_subgroups %>% pmap_dfr(function(spec, spec_filter) {
  tibble(
    spec,
    N=spec_filter(df_experiments) %>% group_by(study) %>% slice_sample(n=1) %>% ungroup %>% with(sum(N_regression))
  )
})

proportion_significant =spec_comparisons %>% pmap_dfr(function(spec_group, spec1, spec2, ...) {
  df_experiments %>%
    filter(is.na(hypothesis), participants %in% c(spec1, spec2)) %>%
    mutate(participants=case_when(participants==spec1~"grp1", participants==spec2~"grp2")) %>%
    unnest(df_reg) %>%
    pivot_wider(id_cols=c(study, outcome.name, hypothesis, reference_condition, condition.name), names_from=participants, values_from=c(estimate, std.error)) %>%
    mutate(
      diff=estimate_grp1-estimate_grp2,
      std.error_diff = sqrt(std.error_grp1^2 + std.error_grp2^2),
      p_diff = 2 * (1-pnorm(abs(diff)/std.error_diff)),
      sig = p_diff<0.05
    ) %>%
    summarize(spec_group, spec1, spec2, sig=mean(sig))
})

df_experiments %>%
  filter(is.na(hypothesis), participants %in% c("black", "white")) %>%
  unnest(df_reg) %>%
  pivot_wider(id_cols=c(study, outcome.name, hypothesis, reference_condition, condition.name), names_from=participants, values_from=c(estimate, std.error)) %>%
  mutate(
    diff=estimate_black-estimate_white,
    std.error_diff = sqrt(std.error_black^2 + std.error_white^2),
    p_diff = 2 * (1-pnorm(abs(diff)/std.error_diff)),
    sig = p_diff<0.05
  ) %>%
  summarize(sig=mean(sig))

sims_subgroups %>%
  group_by(spec, model) %>%
  summarize(across(c(estimate, ci.lb, ci.ub, cor_raw, cor_raw_ci.lb, cor_raw_ci.ub, rmse, rmse_adj, b, b_conf.low, b_conf.high), ~median(., na.rm=T))) %>%
  mutate(across(c(rmse, rmse_adj), ~.*100)) %>%
  ungroup() %>%
  pivot_longer(-c(model, spec)) %>%
  transmute(name =
              str_glue("subgroups_{model}_{spec}_{name}") %>%
              str_replace_all('[^a-zA-Z0-9-]', '_'),
            value=sprintf("%.2f", value)) %>%
  bind_rows(
    subgroup_N %>% transmute(name=str_glue("subgroups_N_{spec}"), value=prettyNum(N, big.mark=","))
  ) %>%
  bind_rows(
    proportion_significant %>% transmute(name=str_glue("subgroups_significant_interactions_{spec_group}"), value=sprintf("%.1f", sig*100))
  ) %>%
  save_vars("subgroups.tex")


df_fig_subgroups = sims_subgroups %>%
  group_by(spec, model) %>%
  summarize(across(c(estimate, ci.lb, ci.ub, cor_raw, cor_raw_ci.lb, cor_raw_ci.ub), ~median(., na.rm=T))) %>%
  ungroup() %>%
  filter(model=="gpt-4") %>%
  mutate(
    spec=as.character(spec),
    spec=str_to_title(spec)
  ) %>%
  mutate(spec = spec %>% fct_relevel("Male", "Female", "White", "Black", "Republican", "Democrat") %>% fct_rev) %>%
  mutate(spec_group = case_when(
    spec %in% c("Male", "Female") ~ "Gender",
    spec %in% c("White", "Black") ~ "Ethnicity",
    spec %in% c("Republican", "Democrat") ~ "Party"
  )) %>%
  left_join(proportion_significant, by="spec_group")
pos = position_dodgev(0.5)
g_subgroups = df_fig_subgroups %>%
  mutate(spec_group_label = paste0("<b>", spec_group, "</b><br/><span style='font-size:6.5pt'>", sprintf("%.1f", sig*100), "% <i>sig. interactions</i></span>")) %>%
  ggplot(aes(y=spec, x=estimate, xmin=ci.lb, xmax=ci.ub, color=fct_rev(model))) +
  facet_grid(spec_group_label~., scale="free", space="free") +
  geom_linerange(alpha=0.5, position=pos) +
  geom_point(aes(shape="adj", alpha="adj"), position=pos) +
  geom_point(aes(x=cor_raw, shape="_raw", alpha="_raw"), size=1.7, stroke=0.7, position=pos) +
  geom_text(aes(label=sprintf("%.2f", estimate)), vjust=-0.8, size=2.7) +
  scale_shape_manual(name=NULL, values=c("_raw"=4, "adj"=16), labels=c("_raw"="Raw correlation (_r_ )", "adj"="Correlation adjusting for effect size uncertainty (_r<sub>adj</sub>_ )")) +
  scale_alpha_manual(name=NULL, values=c("_raw"=0.3, "adj"=1), labels=c("_raw"="Raw correlation (_r_ )", "adj"="Correlation adjusting for effect size uncertainty (_r<sub>adj</sub>_ )")) +
  scale_color_manual(values=c("gpt-4"="black", "human"=hue_pal()(3)[[3]]), guide="none") +
  theme_bw() + theme(
    panel.grid.major.y = element_blank(),
    strip.background = element_blank(),
    strip.text.y = element_markdown(),
    axis.title.y = element_blank(),
    legend.position="top",
    legend.text = element_markdown()
  ) +
  scale_x_continuous(limits=c(0, 1), expand = c(0,0), oob=scales::oob_squish) +
  labs(x="Correlation between GPT-4 and observed effects")
g_subgroups

ggsave("../results/plots/subgroups.pdf", g_subgroups, width=5.5, height=4.2)




# Function for calculating whether demographic differences are statistically significant
calculate_comparisons = function() {
  # comparison_stats = with_progress({p=progressor(nrow(spec_comparisons)*n_runs); spec_comparisons %>% crossing(run_idx=1:n_runs) %>% pmap_dfr(function(spec_group, spec1, spec2, run_idx, extra_globals) {
  comparison_stats = with_progress({p=progressor(nrow(spec_comparisons)*n_runs); spec_comparisons %>% crossing(run_idx=1:n_runs) %>% future_pmap_dfr(
    .options = furrr_options(
      globals = c("specs_subgroups", "d_subgroups", "d_ethnicity", "d_gender", "d_party"),
      packages = c("tidyverse", "Matrix", "metafor")
    ), function(spec_group, spec1, spec2, run_idx, extra_globals) {
    spec_filter1 = specs_subgroups %>% filter(spec==spec1) %>% with(spec_filter[[1]])
    spec_filter2 = specs_subgroups %>% filter(spec==spec2) %>% with(spec_filter[[1]])
    
    samp = spec_filter1(d_subgroups) %>%
      group_by(study) %>%
      filter(reference_condition == sample(reference_condition, 1)) %>%
      filter(outcome.name == sample(outcome.name, 1)) %>%
      ungroup() %>%
      select(study, reference_condition, outcome.name) %>%
      distinct
    
    d_samp1 = spec_filter1(d_subgroups) %>% right_join(samp)
    d_samp2 = spec_filter2(d_subgroups) %>% right_join(samp)
    
    df_est1 = d_samp1 %>% unnest(c(df_reg, df_llm), names_sep=".")
    df_est2 = d_samp2 %>% unnest(c(df_reg, df_llm), names_sep=".")
    stopifnot(all(df_est1$df_reg.condition.name==df_est1$df_llm.condition.name))
    stopifnot(all(df_est2$df_reg.condition.name==df_est2$df_llm.condition.name))
    
    y1 = df_est1$df_reg.estimate
    p1 = df_est1$df_llm.prediction
    y2 = df_est2$df_reg.estimate
    p2 = df_est2$df_llm.prediction
    
    V1 = bdiag(d_samp1$V)
    V2 = bdiag(d_samp2$V)
    V.pred = diag(0, nrow=length(y1))
    V_all = bdiag(V1, V2, V.pred, V.pred)
    data = tibble(est1=y1, est2=y2, pred1=p1, pred2=p2) %>% mutate(id=row_number()) %>% pivot_longer(-id) %>% arrange(name)
    
    fit = tryCatch(rma.mv(
        data = data,
        value ~ 0 + name,
        random = list(~name|id),
        V  = V_all,
        method = "REML",
        struct="UN", sparse=T,
        cvvc=TRUE, control=list(hesspack="pracma", iter.max=1000, rel.tol=1e-8)
      ), error = function(e) { message("Failed to fit corr regression! ", conditionMessage(e)); NULL })
    if (is.null(fit)) { p(); return(tibble()) }
    fit
    
    out = tibble(
      run_idx, spec1, spec2,
      corr_ates = fit$rho[[1]],
      corr_preds = fit$rho[[6]],
      corr_diff_estimate = fit$rho[[2]] - fit$rho[[5]],
    )
    if(! any(is.na(fit$vvc))) {
      out = out %>% mutate(
        corr_diff_se = sqrt(fit$vvc['rho.3.1', 'rho.3.1'] + fit$vvc['rho.4.2', 'rho.4.2'] - 2 * fit$vvc['rho.3.1', 'rho.4.2']),
        corr_diff_lwr = corr_diff_estimate - qnorm(0.975) * corr_diff_se,
        corr_diff_upr = corr_diff_estimate + qnorm(0.975) * corr_diff_se,
        corr_diff_p = 2*(1-pnorm(abs(corr_diff_estimate) / corr_diff_se)),
      )
    }
    
    
    ## MSE New
    
    data = tibble(err1=y1-p1, err2=y2-p2) %>% mutate(id=row_number()) %>% pivot_longer(-id) %>% arrange(name)
    V = bdiag(V1, V2)
    fit = tryCatch(rma.mv(
        data = data,
        value ~ 0 + name,
        random = list(~name|id),
        V  = V,
        method = "REML",
        struct="UN", sparse=T,
        cvvc=TRUE, control=list(hesspack="pracma", iter.max=1000, rel.tol=1e-8)
      ), error = function(e) { message("Failed to fit MSE regression! ", conditionMessage(e)); NULL })
    if (is.null(fit)) { p(); return(tibble()) }
    fit
    
    out = out %>% mutate(
      run_idx, spec_group, spec1, spec2,
      mse_diff_estimate = fit$tau2[[1]] - fit$tau2[[2]]
    )
    if(! any(is.na(fit$vvc))) {
      out = out %>% mutate(
        mse_diff_se = sqrt(fit$vvc['tau^2.1', 'tau^2.1'] + fit$vvc['tau^2.2', 'tau^2.2'] - 2 * fit$vvc['tau^2.1', 'tau^2.2']),
        mse_diff_lwr = mse_diff_estimate - qnorm(0.975) * mse_diff_se,
        mse_diff_upr = mse_diff_estimate + qnorm(0.975) * mse_diff_se,
        mse_diff_p = 2*(1-pnorm(abs(mse_diff_estimate) / mse_diff_se)),
      )
    }
    p()
    out
  })})
  
  print(nrow(comparison_stats))
  
  comparison_stats %>%
    group_by(spec1, spec2) %>%
    summarize(across(c(corr_ates, corr_preds, corr_diff_estimate, corr_diff_lwr, corr_diff_upr, corr_diff_p, mse_diff_estimate, mse_diff_lwr, mse_diff_upr, mse_diff_p), median)) %>%
    ungroup()
  
  comparison_stats %>%
    group_by(spec1, spec2) %>%
    summarize(across(c(corr_ates, corr_preds, corr_diff_estimate, corr_diff_lwr, corr_diff_upr, corr_diff_p, mse_diff_estimate, mse_diff_lwr, mse_diff_upr, mse_diff_p), median)) %>%
    ungroup() %>%
    mutate(across(c(corr_ates, corr_preds, corr_diff_estimate, corr_diff_lwr, corr_diff_upr), ~sprintf("%.2f", .)),
           across(c(corr_diff_p, mse_diff_p), ~sprintf("%.3f", .)),
           across(c(mse_diff_estimate, mse_diff_lwr, mse_diff_upr), ~sprintf("%.5f", .))
    ) %>%
    pivot_longer(-c(spec1, spec2)) %>%
    transmute(name=str_glue("{spec1}_{spec2}_{name}"), value) %>%
    save_vars("subgroup_comparisons.tex")
}
calculate_comparisons()