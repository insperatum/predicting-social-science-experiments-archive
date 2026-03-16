library(tidyverse)
library(progressr)
library(furrr)
library(ggtext)
library(ggstance)
library(scales)
library(patchwork)
source("util.R")
source("config.R")
source("load_archive1_results.R")


#### Raw effect sizes (Figure 2A) ####
# Get the effect sizes
d_2a = df_experiments %>%
  inner_join(study_features) %>%
  inner_join(df_predictions) %>%
  unnest(df_reg) %>%
  unnest(df_llm, names_sep="__") %>%
  filter(condition.name == df_llm__condition.name) %>% select(-df_llm__condition.name) %>% rename(prediction = df_llm__prediction) %>%
  filter(participants=="all", is.na(hypothesis), model=="gpt-4")


d_2a %>% 
  mutate(across(c(estimate, prediction), ~.*sign(prediction))) %>%
  mutate(pred_quantile=cut(prediction, quantile(prediction, 0:6/6))) %>%
  filter(!is.na(pred_quantile)) %>%
  group_by(pred_quantile) %>%
  summarize(x=mean(prediction), y=mean(p.value<0.05)) %>%
  ggplot(aes(x=x, y=y, label=paste0("LLM prediction: ", pred_quantile))) +
  geom_text(vjust=0.5, hjust=-0.05, size=2.5) +
  # geom_line() +
  geom_point() +
  theme_bw() +
  coord_cartesian(ylim=c(0,1), xlim=c(0,1)) +
  labs(x="LLM prediction", y="Proportion significant")
# Make the figure
g_2a = d_2a %>% 
  mutate(grp=ifelse(study_published_by_2021, "published", "unpublished") %>% fct_rev) %>%
  arrange(fct_rev(grp)) %>%
  group_by(study) %>%
  ggplot(aes(y=prediction, x=estimate)) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_vline(xintercept=0, linetype="dashed") +
  geom_point(aes(size=grp, alpha=grp, color=grp), stroke=0) +
  scale_size_manual(values=c("unpublished"=0.7, "published"=0.7), labels=c("unpublished"="Unpublished by 2021", "published"="Published by 2021"), guide="none") +
  scale_alpha_manual(values=c("unpublished"=0.7, "published"=0.7), labels=c("unpublished"="Unpublished by 2021", "published"="Published by 2021")) +
  scale_color_manual(values=c("unpublished"="black",
                              "published"="gray80"
                              ), labels=c("unpublished"="Unpublished by 2021", "published"="Published by 2021")) +  
  theme_bw() + theme(
    legend.position="top", legend.title=element_blank()
  )
g_2a





#### Accuracy by model (Figure 2B) ####
# Run the simulations (takes ~30mins on my 2024 macbook pro)
sims_2b = df_experiments %>%
  inner_join(df_predictions) %>%
  run_sims(tibble(spec="all", spec_filter=list(\(x) filter(x, participants=="all", is.na(hypothesis)))))

# Take a median across runs
sims_avg_2b = sims_2b %>%
  group_by(model) %>%
    summarize(across(c(estimate, ci.lb, ci.ub, cor_raw, cor_raw_ci.lb, cor_raw_ci.ub, rmse, rmse_adj, b, b_conf.low, b_conf.high), ~median(., na.rm=T))) %>%
    mutate(across(c(rmse, rmse_adj), ~.*100)) %>%
  ungroup()

# Export for latex
sims_avg_2b %>%
  pivot_longer(-model) %>%
  transmute(name =
              str_glue("fig2b_{model}_{name}") %>%
              str_replace_all('[^a-zA-Z0-9-]', '_'),
            value=sprintf("%.2f", value)) %>%
  save_vars("fig2b.tex")

# Calculate statistics for comparisons between models (takes XXX on my 2024 macbook pro)
comparison_sims = df_experiments %>%
  inner_join(df_predictions) %>%
  run_comparison_sims(tibble(spec="all", spec_filter=list(\(x) filter(x, participants=="all", is.na(hypothesis)))))

# Take a median across runs
comparison_sims_avg = comparison_sims %>%
  group_by(comparison) %>%
  summarize(across(c(matches("(beta1)|(beta2)|(mse_diff)|(corr_diff)")), median))

# Export for latex
comparison_sims_avg %>%
  pivot_longer(-comparison, names_pattern="(.*)_([^_]*)$", names_to = c("a", "name")) %>%
  transmute(name =
              str_glue("comparison_{comparison}_{a}_{name}") %>%
              str_replace_all('[^a-zA-Z0-9-]', '_'),
            value=ifelse(str_ends(name, "_p"), sprintf("%.3f", value), sprintf("%.2f", value))) %>%
    save_vars("comparisons.tex")


# Create the figure
df_fig2b = sims_avg_2b %>%
  mutate(model_class = case_when(
    model %in% c("babbage-002", "davinci-002", "gpt-3.5-turbo", "gpt-4") ~"gpt",
    model %in% c("google/gemma-3-27b-it", "deepseek/deepseek-chat-v3-0324", "openai/gpt-oss-120b") ~ "open",
    model %in% c("human","combined") ~ "human") %>% fct_relevel("gpt", "open", "human")) %>%
  mutate(Model = c(
    # "babbage-002"="GPT-3<br>(Babbage)",
    "davinci-002"="GPT-3<br>(Davinci)",
    "gpt-3.5-turbo"="GPT-3.5",
    "gpt-4"="<b>GPT-4</b>",
    "human"="<span style='color:#619Cff'>Human<br>forecasters</span>",
    "google/gemma-3-27b-it"="<span style='color:#00BA38'>Gemma-3<br>(27B)</span>",
    "deepseek/deepseek-chat-v3-0324"="<span style='color:#00BA38'>Deepseek-v3</span>",
    "openai/gpt-oss-120b"="<span style='color:#00BA38'>GPT-OSS<br>(120B)</span>",
    "combined"="<span style='color:#619Cff'>Human<br>forecasters<br>+ GPT-4</span>"
  )[model] %>% fct_reorder(estimate) %>% fct_reorder(as.integer(model_class))) %>%
  filter(!is.na(Model))

pos = position_dodgev(0.5)
g_2b = df_fig2b %>%
  mutate(ModelClass=c("gpt"="GPT", "open"="Open-weight", "human"="")[model_class] %>% fct_reorder(as.integer(model_class))) %>%
  ggplot(aes(y=fct_rev(Model), x=estimate, xmin=ci.lb, xmax=ci.ub, color=model_class)) +
  facet_grid(ModelClass~., scale="free", space="free") +
  geom_linerange(alpha=0.5, position=pos) +
  geom_point(aes(shape="adj", alpha="adj"), position=pos) +
  geom_point(aes(x=cor_raw, shape="_raw", alpha="_raw"), size=1.7, stroke=0.7, position=pos) +
  geom_text(aes(label=sprintf("%.2f", estimate)), vjust=-0.8, size=2.7) +
  scale_shape_manual(name=NULL, values=c("_raw"=4, "adj"=16), labels=c("_raw"="Raw correlation (_r_ )", "adj"="Correlation adjusting for effect size uncertainty (_r<sub>adj</sub>_ )")) +
  scale_alpha_manual(name=NULL, values=c("_raw"=0.3, "adj"=1), labels=c("_raw"="Raw correlation (_r_ )", "adj"="Correlation adjusting for effect size uncertainty (_r<sub>adj</sub>_ )")) +
  scale_color_manual(values=c("gpt"="black",
                              "human"=hue_pal()(3)[[3]],
                              "open"=hue_pal()(3)[[2]]
  ), guide="none") +
  theme_bw() + theme(
    panel.grid.major.y = element_blank(),
    strip.background = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_markdown(),
    strip.text = element_markdown(),
    legend.position="none"
  ) +
  scale_x_continuous(limits=c(0, 1), expand = c(0,0), oob=oob_squish) +
  labs(x="Correlation")
g_2b




#### Accuracy by specification / robustness checks (Figure 2C) ####
data_2c = df_experiments %>%
  left_join(study_features) %>%
  left_join(outcome_features) %>%
  inner_join(df_predictions %>% filter(model %in% c("gpt-4", "human", "combined")))

specs_2c = tribble(
  ~spec, ~spec_filter,
  "all", \(x) filter(x, participants=="all", is.na(hypothesis)),
  "unpublished by 2021", \(x) filter(x, participants=="all", !study_published_by_2021, is.na(hypothesis)),
  "published by 2021", \(x) filter(x, participants=="all", study_published_by_2021, is.na(hypothesis)),
  "published post 2021", \(x) filter(x, participants=="all", study_published_after_2021, is.na(hypothesis)),
  "never published", \(x) filter(x, participants=="all", study_never_published, is.na(hypothesis)),
  "tess", \(x) filter(x, participants=="all", study_is_tess, is.na(hypothesis)),
  "coppock/mullinix", \(x) filter(x, participants=="all", !study_is_tess, is.na(hypothesis)),
  "hypothesized", \(x) filter(x, participants=="all", !is.na(hypothesis)),
  "existing_attitudes", \(x) filter(x, participants=="all", outcome_existing_attitude, is.na(hypothesis)),
  "sociology", \(x) filter(x, participants=="all", study_is_sociology, is.na(hypothesis)),
  "political_science", \(x) filter(x, participants=="all", study_is_political_science, is.na(hypothesis)),
  "communication", \(x) filter(x, participants=="all", study_is_communication, is.na(hypothesis)),
  "psychology", \(x) filter(x, participants=="all", study_is_psychology, is.na(hypothesis)),
  "social_policy", \(x) filter(x, participants=="all", study_is_social_policy, is.na(hypothesis)),
  "unrecognized author", \(x) filter(x, participants=="all", !study_recognize_author, is.na(hypothesis)),
  "small_effects", function(x) {
    x %>%
      filter(participants=="all", is.na(hypothesis)) %>%
      mutate(
        keep_idxs = map(df_reg, \(x) abs(x$estimate) < 0.05),
        df_reg = map2(df_reg, keep_idxs, \(x,y) x[y,]),
        V = map2(V, keep_idxs, \(x, y) as.matrix(x)[y,y])
      ) %>%
      filter(map_lgl(df_reg, \(x) nrow(x)>0))
  },
  "large_effects", function(x) {
    x %>%
      filter(participants=="all", is.na(hypothesis)) %>%
      mutate(
        keep_idxs = map(df_reg, \(x) abs(x$estimate) >= 0.05),
        df_reg = map2(df_reg, keep_idxs, \(x,y) x[y,]),
        V = map2(V, keep_idxs, \(x, y) as.matrix(x)[y,y])
      ) %>%
      filter(map_lgl(df_reg, \(x) nrow(x)>0))
  }
)

# Run the simulations (takes ~15mins on my 2024 macbook pro)
sims_2c = run_sims(data_2c, specs_2c) 
# Take a median across runs
sims_avg_2c = sims_2c %>%
  group_by(spec, model) %>%
    summarize(across(c(estimate, se, ci.lb, ci.ub, cor_raw, cor_raw_ci.lb, cor_raw_ci.ub, rmse, rmse_adj, mse_adj_se, b, b_conf.low, b_conf.high), ~median(., na.rm=T))) %>%
    mutate(across(c(rmse, rmse_adj), ~.*100),
           across(mse_adj_se, ~.*10000)
           ) %>%
  ungroup()

# Export for latex
sims_avg_2c %>%
  pivot_longer(-c(model, spec)) %>%
  transmute(name =
              str_glue("fig2c_{model}_{spec}_{name}") %>%
              str_replace_all('[^a-zA-Z0-9-]', '_'),
            value=sprintf("%.2f", value)) %>%
  save_vars("fig2c.tex")

# Make the figure
df_fig2c = sims_avg_2c %>%
  filter(model=="gpt-4") %>%
  mutate(grp = case_when(
    spec %in% c("sociology", "communication", "psychology", "political_science", "social_policy") ~ "Field",
    T~""
  ) %>% fct_relevel("", "Study", "Effect", "Field")) %>%
  filter(
    !spec %in% c("published post 2021", "published by 2021"),
    !spec %in% c("tess", "coppock/mullinix")
    ) %>%
  mutate(
    spec=as.character(spec),
    spec = c(
      "all"="All (random contrasts)",
      "hypothesized"="Hypothesized contrasts",
      "sociology"="Sociology",
      "communication"="Communication",
      "psychology"="Psychology",
      "political_science"="Political Science",
      "social_policy"="Social Policy",
      "never published"="Unpublished by 2025",
      "unpublished by 2021"="Unpublished by 2021",
      "tess"="Source: TESS",
      "coppock/mullinix"="Source: Replication archives",
      "unrecognized author"="Study author not recognized",
      "existing_attitudes"="Targets existing attitudes",
      "small_effects"="Small effects"
    )[spec] %>% coalesce(spec)
  ) %>%
  mutate(spec = spec %>% fct_reorder(-estimate) %>% fct_relevel("Random contrasts", "Source: TESS", "Source: Replication archives") %>% fct_rev)
  
pos = position_dodgev(0.5)
g_2c = df_fig2c %>%
  filter(spec!="All (random contrasts)", spec!="large_effects") %>%
  mutate(spec=fct_reorder(spec, -as.integer(grp))) %>%
  ggplot(aes(y=spec, x=estimate, xmin=ci.lb, xmax=ci.ub, color=fct_rev(model))) +
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
    axis.title.y = element_blank(),
    legend.position="none"
  ) +
  scale_x_continuous(limits=c(0, 1), expand = c(0,0), oob=oob_squish) +
  labs(x="Correlation")
g_2c






#### Combine figures 2A,2B, 2C together ####
g = (
  (g_2a +
     scale_x_continuous(limits=c(-0.7, 0.7), breaks=seq(-1, 1, 0.5), expand=c(0,0)) +
     scale_y_continuous(limits=c(-1, 1), breaks=seq(-1, 1, 0.5), expand=c(0, 0)) +
     ggtitle("**A. All effects (GPT-4)**") + 
     labs(x="Observed effect size", y="Predicted effect size") +
     theme(
    plot.title = element_markdown(margin = margin(b=15)),
    legend.margin = margin(3,3,3,3,"pt"),
    legend.position = c(0.02,0.98),
    legend.justification = c(0,1),
    legend.text = element_text(size=7, margin=margin(0,0,0,0,"pt")),
    legend.key.height = unit(10, "pt"),
    legend.key.width = unit(4, "pt"),
    legend.background = element_rect(color="black", linewidth=0.1),
    legend.title = element_blank(),
    legend.spacing.y = unit(0, "pt"),
    )) + 
    plot_spacer() +
    (g_2b + ggtitle("**B. Model comparison**") +
       labs(x="Correlation with observed effects") +
       theme(
         plot.title = element_markdown(),
         legend.position="none"
       )
    ) +
    plot_spacer() +
  (g_2c + ggtitle("**C. Robustness checks (GPT-4)**") +
     labs(x="Correlation with observed effects") +
     theme(
       axis.title.y = element_blank(),
       axis.title.x = element_text(hjust=1),
       plot.title.position = "plot",
       plot.title = element_markdown(margin=margin(0,0,22,0), hjust=1),
       
       # legend.position="none"
       
       legend.margin = margin(-3, 4, 1, 5),
       legend.justification = "right",
       legend.position=c(1, 1.04),
       legend.key.width = unit(2, "pt"),
       legend.text = element_markdown(size=8, margin=margin(0,0.01,0,0,"pt")),
       legend.direction="horizontal",
       legend.background = element_blank(),
       legend.box.background = element_blank(),
       )
      
   )
) +
  patchwork::plot_layout(widths=c(2.2,0,2,0,2))
g
ggsave("../results/plots/primary_archive.pdf", g, width=11.5, height=4.5)




#### Supplementary plot with regression slope ####

((
df_fig2b %>%
  mutate(ModelClass=c("gpt"="GPT", "open"="Open-weight", "human"="")[model_class] %>% fct_reorder(as.integer(model_class))) %>%
  ggplot(aes(y=fct_rev(Model), x=b, xmin=pmax(b_conf.low, 0), xmax=b_conf.high, color=model_class)) +
  facet_grid(ModelClass~., scale="free", space="free") +
  geom_vline(xintercept=1, linetype="dashed") +
  geom_linerange(alpha=0.5, position=pos) +
  geom_point() +
  geom_text(aes(label=sprintf("%.2f", b)), vjust=-0.8, size=2.7) +
  scale_color_manual(values=c("gpt"="black",
                              "human"=hue_pal()(3)[[3]],
                              "open"=hue_pal()(3)[[2]]
  ), guide="none") +
  theme_bw() + theme(
    panel.grid.major.y = element_blank(),
    strip.background = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_markdown(),
    strip.text = element_markdown(),
    legend.position="none"
  ) +
  scale_x_continuous(limits=c(0,1.8), expand=expansion(0)) +
  labs(x="Regression slope") +
  ggtitle("Regression slope by model")
) +

(
df_fig2c %>%
  filter(spec!="All (random contrasts)", spec!="large_effects") %>%
  mutate(spec=fct_reorder(spec, -as.integer(grp))) %>%
  ggplot(aes(y=spec, x=b, xmin=b_conf.low, xmax=b_conf.high)) +
  geom_vline(xintercept=1, linetype="dashed") +
  geom_linerange(alpha=0.5) +
  geom_point() +
  geom_text(aes(label=sprintf("%.2f", b)), vjust=-0.8, size=2.7) +
  theme_bw() + theme(
    panel.grid.major.y = element_blank(),
    strip.background = element_blank(),
    axis.title.y = element_blank(),
    legend.position="none"
  ) +
  scale_x_continuous(limits=c(0,1.1), expand=expansion(0)) +
  labs(x="Regression slope (GPT-4)") +
  ggtitle("Regression slope by effect type")
)
)%>%
  ggsave("../results/plots/regression_slope.pdf", ., width=9, height=4.5)
