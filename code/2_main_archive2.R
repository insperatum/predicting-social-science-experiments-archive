# Takes approximately 1 hour

library(tidyverse)
library(progressr)
library(furrr)
library(scales)
library(ggstance)
library(ggtext)
library(ggnewscale)
source("util.R")
source("config.R")
df_studies_outcomes = readRDS("../data/megastudies.RDS")

cat ("Analysing", nrow(df_studies_outcomes), "megastudy x outcomes\n")
return_profile = T; return_ci = T
results_by_study_outcome = run_cached("output/processed_data/megastudy_profiles.rds", \() with_progress({
  p=progressor(nrow(df_studies_outcomes))
  
  df_studies_outcomes %>% mutate(i=row_number()) %>% future_pmap_dfr(
    function(i, dataset, outcome, df, V, N, ...) {
      cat(dataset, " ", outcome, "\n")
    # Calculate correlation between RCT data and GPT-4
    corr = calc_correlation(df$estimate.rct, V, df$`prediction.gpt-4`, return_ci = return_ci, return_profile = return_profile)
    out = tibble(dataset, outcome,
                  r_gpt = corr[['cor_raw']],
                  r_gpt.lower = corr[['cor_raw_ci.lb']],
                  r_gpt.upper = corr[['cor_raw_ci.ub']],
                  r_adj_gpt = corr[['estimate']],
                  r_adj_gpt.lower = if(return_ci) corr[['ci.lb']],
                  r_adj_gpt.upper = if(return_ci) corr[['ci.ub']],
                  profile_gpt = if(return_profile) corr[['profile']]
                  )
    
    # Calculate correlation between RCT and expert forecasts
    if("prediction.expert" %in% colnames(df)) {
      corr = calc_correlation(df$estimate.rct, V, df$`prediction.expert`, return_ci = return_ci, return_profile = return_profile)
      
      out = out %>% mutate(
        r_expert = corr[['cor_raw']],
        r_expert.lower = corr[['cor_raw_ci_lb']],
        r_expert.upper = corr[['cor_raw_ci_ub']],
        r_adj_expert = corr[['estimate']],
        r_adj_expert.lower = if(return_ci) corr[['ci.lb']],
        r_adj_expert.upper = if(return_ci) corr[['ci.ub']],
        profile_expert = if(return_profile) corr[['profile']],
      )
      
      comp = calc_correlation_comparison(df$estimate.rct, V, df$`prediction.gpt-4`, df$prediction.expert)
      out = out %>% mutate(
        r_diff = comp[['corr_diff_estimate']],
        r_diff.lower = comp[['corr_diff_lwr']],
        r_diff.upper = comp[['corr_diff_upr']],
        r_diff_se = comp[['corr_diff_se']]
      )
    }
    
    p()
    out
  })
}))




profile_mean_rho = function(profs, lambda_grid = seq(-50, 50, length.out = 2001)) {
  pick_opt = function(tbl, lam) {
    idx = which.max(tbl$ll + lam * tbl$rho)
    c(rho = tbl$rho[idx], ll = tbl$ll[idx])
  }
  
  prof_curve = #with_progress({p = progressor(steps = length(lambda_grid)); 
    map_dfr(lambda_grid, function(lam) {
    # p()
    opts = map(profs, pick_opt, lam = lam) %>% transpose()
    rhos = unlist(opts$rho)
    lls  = unlist(opts$ll)
    
    tibble(lambda  = lam,
           mean_rho = mean(rhos),
           ll       = sum(lls))
  # })
  }) %>%
    arrange(mean_rho) %>%
    distinct(mean_rho, .keep_all = TRUE) %>%
    mutate(ll_rel = ll - max(ll))
  
  cutoff = -qchisq(.95, df = 1)/2
  
  ci = prof_curve %>%
    filter(ll_rel >= cutoff) %>%
    summarise(ci_low  = min(mean_rho), ci_high = max(mean_rho))
  
  tibble(
    profile=list(prof_curve),
    estimate=prof_curve %>% slice_max(ll) %>% with(mean_rho[[1]]),
    conf.low = ci$ci_low[[1]],
    conf.high = ci$ci_high[[1]]
  )
}


cat("Averaging results for each megastudy group\n")
results_by_dataset_outcome = run_cached("output/processed_data/megastudies_by_dataset_outcome.rds", \() {
  results_by_study_outcome %>%
    mutate(
      dataset_type = case_when(
        startsWith(dataset, "Broockman") ~ "Broockman",
        startsWith(dataset, "Tappin") ~ "Tappin",
        T ~ dataset
      )
    ) %>%
    group_by(dataset_type, outcome) %>%
    group_modify(function(d,k) {
      print(k)
      dd = profile_mean_rho(d$profile_gpt) %>%
          transmute(estimate.gpt=estimate, conf.low.gpt=conf.low, conf.high.gpt=conf.high, r.gpt=mean(d$r_gpt), profile.gpt=profile)

      if(!all(is.na(d$r_expert))) {
        dd = dd %>% bind_cols(
          profile_mean_rho(d$profile_expert) %>%
            transmute(estimate.expert=estimate, conf.low.expert=conf.low, conf.high.expert=conf.high, r.expert=mean(d$r_expert), profile.expert=profile),
        )
      }
      dd
    }) %>%
    ungroup
})



dataset_properties = results_by_dataset_outcome %>%
  select(dataset_type) %>%
  distinct %>%
  mutate(
    intervention_delivery = case_when(
      dataset_type %in% c("Saccardo2024", "Milkman2021", "Milkman2022", "Milkman2023", "Dellavigna") ~ "Field experiments",
      T ~ "Survey experiments"
    ),
    is_text = case_when(
      dataset_type %in% c("Milkman2021", "Milkman2023", "SDC", "Tappin") ~ "Nontext",
      T ~ "Text"
    ),
    study_label = c(
      "Allen2023"="Allen et al. 2024",
      "Broockman"="Broockman et al. 2024",
      "Dellavigna"="Dellavigna & Linos 2022",
      "DellavignaPope"="Dellavigna & Pope 2018",
      "Doell"="Vlasceanu et al. 2024",
      "Goldwert"="Goldwert et al. 2026",
      "Mason"="Mason et al. 2025",
      "Milkman2021"="Milkman et al. 2021",
      "Milkman2022"="Milkman et al. 2022",
      "Milkman2023"="Milkman et al. 2024",
      "SDC"="Voelkel et al. 2024",
      "Saccardo2024"="Saccardo et al. 2024",
      "Tappin"="Tappin et al. 2023",
      "Voelkel2025"="Voelkel et al. 2025",
      "Zickfeld"="Zickfeld et al. 2025"
    )[dataset_type],
    has_expert_forecasts = dataset_type %in% c("Broockman", "Dellavigna", "DellavignaPope", "Doell", "Zickfeld", "SDC", "Milkman2022", "Milkman2021", "Milkman2023"),
    outcome_label = c(
      "Allen2023"="COVID-19 attitudes",
      "Broockman"="Political attitudes",
      "Dellavigna"="Various behavioural outcomes",
      "DellavignaPope"="Typing task effort",
      "Doell"="Climate-related attitudes",
      "Goldwert"="Climate-related attitudes",
      "Mason"="Intention to vote",
      "Milkman2021"="Gym attendance",
      "Milkman2022"="Flu vaccination",
      "Milkman2023"="COVID-19 vaccination",
      "SDC"="Democracy-related attitudes",
      "Saccardo2024"="COVID-19 vaccination",
      "Tappin"="UBI & Immigration policy support",
      "Voelkel2025"="Climate-related attitudes",
      "Zickfeld"="Tax compliance"
    )[dataset_type]
    
  )


# Calculate the meta-analytic averages for different subsets of studies
metaregs_all = run_cached("output/processed_data/megastudy_averages.rds", \() {
  groups = results_by_dataset_outcome %>%
    group_split(dataset_type, .keep = TRUE) %>%
    set_names(unique(results_by_dataset_outcome$dataset_type))
  combo_dfs = crossing(!!!map(groups, ~1:nrow(.))) %>% pmap(function(...) {
    map2(groups, list(...), ~ slice(.x, .y)) %>% 
      bind_rows(.id = "source_tbl")
  })
  
  print("Running metaanalysis: GPT-4 all studies")
  metaregression_all_gpt = with_progress({
    p = progressor(steps=length(combo_dfs))
    combo_dfs %>% map_dfr(function(d) {
      d_subset = d
      r_ci = lm(r.gpt~1,d_subset) %>% broom::tidy(conf.int=T)
      out = profile_mean_rho(map(d_subset$profile.gpt, ~rename(., rho=mean_rho))) %>%
        mutate(r=r_ci$estimate[[1]], r.conf.low=r_ci$conf.low[[1]], r.conf.high=r_ci$conf.high[[1]])
      p(); out
    })
  }) %>% summarize(across(-profile, median))
  
  print("Running metaanalysis: GPT-4 survey experiments")
  metaregression_survey_gpt = with_progress({
    p = progressor(steps=length(combo_dfs))
    combo_dfs %>% map_dfr(function(d) {
      d_subset = d %>% left_join(dataset_properties) %>% filter(intervention_delivery=="Survey experiments")
      r_ci = lm(r.gpt~1,d_subset) %>% broom::tidy(conf.int=T)
      out = profile_mean_rho(map(d_subset$profile.gpt, ~rename(., rho=mean_rho))) %>%
        mutate(r=r_ci$estimate[[1]], r.conf.low=r_ci$conf.low[[1]], r.conf.high=r_ci$conf.high[[1]])
      p(); out
    })
  }) %>% summarize(across(-profile, median))
  
  print("Running metaanalysis: GPT-4 field experiments")
  metaregression_field_gpt = with_progress({
    p = progressor(steps=length(combo_dfs))
    combo_dfs %>% map_dfr(function(d) {
      d_subset = d %>% left_join(dataset_properties) %>% filter(intervention_delivery=="Field experiments")
      r_ci = lm(r.gpt~1,d_subset) %>% broom::tidy(conf.int=T)
      out = profile_mean_rho(map(d_subset$profile.gpt, ~rename(., rho=mean_rho))) %>%
        mutate(r=r_ci$estimate[[1]], r.conf.low=r_ci$conf.low[[1]], r.conf.high=r_ci$conf.high[[1]])
      p(); out
    })
  }) %>% summarize(across(-profile, median))
  
  print("Running metaanalysis: GPT-4 text experiments")
  metaregression_text_gpt = with_progress({
    p = progressor(steps=length(combo_dfs))
    combo_dfs %>% map_dfr(function(d) {
      d_subset = d %>% left_join(dataset_properties) %>% filter(is_text=="Text")
      r_ci = lm(r.gpt~1,d_subset) %>% broom::tidy(conf.int=T)
      out = profile_mean_rho(map(d_subset$profile.gpt, ~rename(., rho=mean_rho))) %>%
        mutate(r=r_ci$estimate[[1]], r.conf.low=r_ci$conf.low[[1]], r.conf.high=r_ci$conf.high[[1]])
      p(); out
    })
  }) %>% summarize(across(-profile, median))
  
  print("Running metaanalysis: GPT-4 nontext experiments")
  metaregression_nontext_gpt = with_progress({
    p = progressor(steps=length(combo_dfs))
    combo_dfs %>% map_dfr(function(d) {
      d_subset = d %>% left_join(dataset_properties) %>% filter(is_text=="Nontext")
      r_ci = lm(r.gpt~1,d_subset) %>% broom::tidy(conf.int=T)
      out = profile_mean_rho(map(d_subset$profile.gpt, ~rename(., rho=mean_rho))) %>%
        mutate(r=r_ci$estimate[[1]], r.conf.low=r_ci$conf.low[[1]], r.conf.high=r_ci$conf.high[[1]])
      p(); out
    })
  }) %>% summarize(across(-profile, median))
  
  print("Running metaanalysis: GPT-4 experiments with forecasts")
  metaregression_hasexpert_gpt = with_progress({
    p = progressor(steps=length(combo_dfs))
    combo_dfs %>% map_dfr(function(d) {
      d_subset = d %>% filter(!is.na(r.expert))
      r_ci = lm(r.gpt~1,d_subset) %>% broom::tidy(conf.int=T)
      out = profile_mean_rho(map(d_subset$profile.gpt, ~rename(., rho=mean_rho))) %>%
        mutate(r=r_ci$estimate[[1]], r.conf.low=r_ci$conf.low[[1]], r.conf.high=r_ci$conf.high[[1]])
      p(); out
    })
  }) %>% summarize(across(-profile, median))
  
  print("Running metaanalysis: expert experiments with forecasts")
  metaregression_hasexpert_expert = with_progress({
    p = progressor(steps=length(combo_dfs))
    combo_dfs %>% map_dfr(function(d) {
      d_subset = d %>% filter(!is.na(r.expert))
      r_ci = lm(r.expert~1,d_subset) %>% broom::tidy(conf.int=T)
      out = profile_mean_rho(map(d_subset$profile.expert, ~rename(., rho=mean_rho))) %>%
        mutate(r=r_ci$estimate[[1]], r.conf.low=r_ci$conf.low[[1]], r.conf.high=r_ci$conf.high[[1]])
      p(); out
    })
  }) %>% summarize(across(-profile, median))
  
  bind_rows(
    metaregression_all_gpt %>% mutate(reg="all_gpt"),
    metaregression_survey_gpt %>% mutate(reg="survey_gpt"),
    metaregression_field_gpt %>% mutate(reg="field_gpt"),
    metaregression_text_gpt %>% mutate(reg="text_gpt"),
    metaregression_nontext_gpt %>% mutate(reg="nontext_gpt"),
    metaregression_hasexpert_gpt %>% mutate(reg="hasexpert_gpt"),
    metaregression_hasexpert_expert %>% mutate(reg="hasexpert_expert"),
  )
})


# Plot!
p=position_dodgev(0.7)

open.study = "<br/>**<span style='font-size:9pt'>"
close.study = "</span>**"

d_plot_all = bind_rows(
  results_by_dataset_outcome %>%
    pivot_longer(
      cols = -c(dataset_type, outcome),
      names_to   = c(".value", "name"),
      names_pattern = ("(.*)\\.([a-z]*)")
    ) %>%
    group_by(name, dataset_type) %>%
    summarize(across(c(estimate, conf.low, conf.high, r), median)) %>%
    ungroup() %>%
    left_join(dataset_properties) %>%
    mutate(l=ifelse(has_expert_forecasts, "*", "")) %>%
    mutate(y = str_glue("{open.study}{study_label}{l}{close.study}<br><span style='color:#777777;font-size:7pt'>{outcome_label}</span>") %>% fct_reorder(estimate),
           yfacet = intervention_delivery),
  metaregs_all %>% transmute(
    name = ifelse(endsWith(reg, "_expert"), "expert", "gpt"),
    y = c(
      "all_gpt" = "<b>All experiments</b>",
      "survey_gpt" = "<b>Survey experiments</b>",
      "field_gpt" = "<b>Field experiments</b>",
      "hasexpert_gpt" = "<b>*Experiments with<br>expert forecasts</b>",
      "hasexpert_expert" = "<b>*Experiments with<br>expert forecasts</b>"
    )[reg],
    yfacet = "Average",
    r, estimate, conf.low, conf.high
  )
)


make_archive2_plot = function(d_plot) {
  p = position_dodgev(0.6)
  g = d_plot %>%
    ggplot(aes(y=y, x=estimate, xmin=conf.low, xmax=conf.high, color=name)) +
    facet_grid(fct_rev(yfacet)~., scale="free", space="free") +
    geom_rect(data=tibble(yfacet="Average"), aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf), fill="black", alpha=0.1, inherit.aes=F) +
    geom_vline(xintercept=0, linetype="dashed") +
    scale_color_manual(values=c("gpt"="black", "expert"=hue_pal()(3)[[3]]), guide="none") +
    geom_point(aes(x=r, shape="_raw", alpha="_raw"), size=1.7, stroke=0.7, position=p, ) +
    geom_linerange(aes(color=name), alpha=0.5, position=p) +
    geom_point(aes(color=name, shape="adj", alpha="adj"), position=p) +
    scale_shape_manual(name=NULL, values=c("_raw"=4, "adj"=16), labels=c("_raw"="Raw correlation (_r_ )", "adj"="Correlation adjusting for effect size uncertainty (_r<sub>adj</sub>_ )")) +
    scale_alpha_manual(name=NULL, values=c("_raw"=0.3, "adj"=1), labels=c("_raw"="Raw correlation (_r_ )", "adj"="Correlation adjusting for effect size uncertainty (_r<sub>adj</sub>_ )")) +
    geom_text(
      aes(label=sprintf("%.2f", estimate), color=name, vjust=ifelse(name=="gpt", -0.8, 1.8)),
      size=2.3,
      alpha=0.5,
      position=p
    ) +
    coord_cartesian(xlim=c(-1, 1)) +
    scale_x_continuous(expand=c(0,0)) +
    geom_richtext(
      data=tibble(yfacet="Average"),
      aes(y=0.75,
          x=-0.17,
          color="expert", label="<b>Expert forecasts</b><br>(wisdom-of-the-crowd)"),
      hjust=0.5, vjust=0.5,
      inherit.aes=F,
      size=2.8,
      alpha=0.7,
      fill=NA, label.color=NA
    ) +
    scale_y_discrete(expand=expansion(add=c(0.8,0.8))) +
    ggtitle("\n") + theme_bw() + theme(
      panel.grid.major.y = element_blank(),
      strip.background = element_blank(),
      legend.margin = margin(-3, 4, 1, 5),
      legend.position=c(0.5, 1.03),
      legend.key.width = unit(1, "pt"),
      legend.text = element_markdown(size=8, margin=margin(r=6)),
      legend.direction="horizontal",
      axis.title.y = element_blank(),
      axis.text.y = element_markdown(),
    ) +
    labs(x="Correlation between predicted and measured effects")
  g
}

g = d_plot_all %>%
  filter(name=="gpt" | yfacet=="Average") %>%
  filter(!is.na(y)) %>%
  mutate(y = y %>% fct_reorder(-estimate) %>% fct_relevel("<b>All experiments</b>") %>% fct_rev %>% fct_relevel("<b>*Experiments with<br>expert forecasts</b>")) %>%
  make_archive2_plot
ggsave("../results/plots/megastudies.pdf", g, width=6.3, height=6.3)

g_supp = d_plot_all %>%
  # filter(name=="gpt" | yfacet=="Average") %>%
  filter(!is.na(y)) %>%
  mutate(y = y %>% fct_reorder(-estimate) %>% fct_relevel("<b>All experiments</b>") %>% fct_rev %>% fct_relevel("<b>*Experiments with<br>expert forecasts</b>")) %>%
  make_archive2_plot
g_supp
ggsave("../results/plots/megastudies_supplement.pdf", g_supp, width=6.3, height=8)




## Table ##


df_studies_outcomes %>%
  pmap_dfr(function(dataset, outcome, df, V, N, ...) {
    tibble(dataset, outcome, N_treatments=nrow(df), N_participants=N)
  }) %>%
  group_by(dataset) %>%
  summarize(
    N_outcomes = n(),
    N_treatments = mean(N_treatments),
    N_participants = mean(N_participants)
  ) %>%
  mutate(dataset_type = str_extract(dataset, "[^-]*")) %>%
  group_by(dataset_type) %>%
  summarize(
    N_outcomes = mean(N_outcomes),
    N_treatments = sum(N_treatments),
    N_participants = sum(N_participants),
    N_effects = N_outcomes * N_treatments
    # N_studies = n()
  ) %>%
  left_join(
    d_plot_all %>% filter(!is.na(dataset_type)) %>% pivot_wider(id_cols = dataset_type, names_from=c(name), values_from=c("estimate", "conf.low", "conf.high"))
  ) %>%
  pivot_longer(-dataset_type) %>%
  transmute(
    name=str_glue("archive2_{dataset_type}_{name}"),
    value=ifelse(str_detect(name, "N_"), as.character(value), sprintf("%.2f", value))
  ) %>%
  save_vars("archive2.tex")






#### GPT4-vs-Expert comparison ####
comparison_stats = results_by_study_outcome %>%
  mutate(
    dataset_type = case_when(
      startsWith(dataset, "Broockman") ~ "Broockman",
      startsWith(dataset, "Tappin") ~ "Tappin",
      T ~ dataset
    )
  ) %>%
  group_by(dataset_type, outcome) %>%
  filter(!all(is.na(r_diff))) %>%
  group_modify(function(d,k) {
    if(!any(is.na(d$r_diff_se))) {
      r_diff_se = sqrt(mean(d$r_diff_se^2)/nrow(d))
    } else {
      # (for broockman et al), can't estimate study-level SEs, so be conservative
      r_diff_se = sqrt(var(d$r_diff)/nrow(d))
    }
    tibble(
      r_diff=mean(d$r_diff),
      r_diff_se=r_diff_se
    )
  }) %>%
  group_by(dataset_type) %>%
    summarize(r_diff=mean(r_diff), r_diff_se=mean(r_diff_se)) %>%
  ungroup() %>%
  summarize(r_diff = mean(r_diff),
            r_diff_se = sqrt(mean(r_diff_se^2) / n()),
            r_diff.lower = r_diff - r_diff_se * qnorm(0.975),
            r_diff.upper = r_diff + r_diff_se * qnorm(0.975),
            r_diff_p.value = 2*(1-pnorm(abs(r_diff)/r_diff_se))
  )
comparison_stats

#### Save the variables ####
bind_rows(
  metaregs_all %>%
    pivot_longer(-reg) %>%
    transmute(name =
                str_glue("fig3_{reg}_{name}") %>%
                str_replace_all('[^a-zA-Z0-9-]', '_'),
              value=sprintf("%.2f", value)),
  comparison_stats %>%
    pivot_longer(everything()) %>%
    mutate(name=str_glue("fig3_comparison_expert_vs_gpt_{name}"), value=sprintf("%.2f", value))
) %>%
  save_vars("fig3.tex")






#### Betas ####

dd = bind_rows(
    df_studies_outcomes %>% filter(! dataset %in% c("Zickfeld", "Doell")),
    # NOTE: for these studies, we subtract the control mean expert prediction
    df_studies_outcomes %>% filter(dataset == "Zickfeld") %>% rowwise %>% mutate(df = list(df %>% mutate(estimate.rct=estimate.rct-82.3))),
    df_studies_outcomes %>% filter(dataset == "Doell", outcome=="belief") %>% rowwise %>% mutate(df = list(df %>% mutate(prediction.expert=prediction.expert-61.64557))),
    df_studies_outcomes %>% filter(dataset == "Doell", outcome=="support") %>% rowwise %>% mutate(df = list(df %>% mutate(prediction.expert=prediction.expert-56.64019)))
  ) %>% 
  pmap_dfr(function(dataset, outcome, df, ...) {
    out = tibble(
      dataset,
      outcome,
      df %>%
        lm(estimate.rct ~ 0 + `prediction.gpt-4`, .) %>%
        broom::tidy()
    )
    if("prediction.expert" %in% colnames(df)) {
      out = out %>% bind_cols(
        df %>%
          lm(estimate.rct ~ 0 + `prediction.expert`, .) %>%
          broom::tidy() %>%
          transmute(b_expert=estimate)
      )
    }
    out
  })

df_betas = dd %>%
  mutate(dataset_type = case_when(
    str_detect(dataset, "Tappin") ~ "Tappin",
    str_detect(dataset, "Broockman") ~ "Broockman",
    T ~ dataset
  )) %>%
  mutate(scale_ratio = case_when(
    dataset_type=="Allen2023" ~ 100,
    dataset_type=="Broockman" ~ 1,
    dataset_type=="Dellavigna" ~ 100,
    dataset_type=="DellavignaPope" ~ 1,
    dataset_type=="Doell" ~ 100,
    dataset_type=="Goldwert" ~ 1,
    dataset_type=="Mason" ~ 100,
    dataset_type=="Milkman2021" ~ 7,
    dataset_type=="Milkman2022" ~ 1,
    dataset_type=="Milkman2023" ~ 100,
    dataset_type=="SDC" ~ 100,
    dataset_type=="Saccardo2024" ~ 1,
    dataset_type=="Tappin" ~ 1,
    dataset_type=="Voelkel2025" ~ 100,
    dataset_type=="Zickfeld" ~ 100,
  )) %>%
  mutate(across(c(estimate, std.error), ~ . / scale_ratio)) %>%
  group_by(dataset_type) %>%
  summarize(b=mean(estimate), std.error=mean(std.error), b_expert=mean(b_expert))

bind_rows(
  df_betas %>%
    mutate(study_type="all"),
  df_betas %>%
    mutate(study_type = case_when(
      dataset_type %in% c("Saccardo2024", "Milkman2022", "Dellavigna", "Milkman2023", "Milkman2021") ~ "field",
      T ~ "survey"
    )),
  df_betas %>%
    filter(dataset_type %in% c("DellavignaPope", "Doell", "Zickfeld", "SDC", "Broockman", "Milkman2022", "Dellavigna", "Milkman2023", "Milkman2021")) %>%
    mutate(study_type = "hasexpert"),
  df_betas %>%
    filter(dataset_type %in% c("DellavignaPope", "Doell", "Zickfeld", "SDC", "Broockman", "Milkman2022", "Dellavigna", "Milkman2023", "Milkman2021")) %>%
    transmute(study_type = "hasexpert_expert", dataset_type, b=b_expert),
  df_betas %>%
    mutate(study_type = case_when(
      dataset_type %in% c("Milkman2021", "Milkman2023", "SDC", "Tappin") ~ "nontext",
      T ~ "text"
    ))
) %>%
  group_by(study_type) %>%
  summarize(b=median(b)) %>%
  transmute(
    name = str_glue("archive2_b_{study_type}"),
    value = sprintf("%.2f", b)
  ) %>%
  save_vars("betas_archive2.tex")