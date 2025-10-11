# 2 seconds 

library(tidyverse)
library(patchwork)
library(lme4)
library(purrr)
library(broom)
library(corrr)
library(apaTables)
library(ggtext)

options(scipen = 999)

df = bind_rows(
  read_csv("data/survey_data/survey_tess.csv") |>
    mutate(distribution="TESS", phd_year=as.character(phd_year)),
  read_csv("data/survey_data/survey_sjdm.csv") |>
    mutate(distribution="SJDM", phd_year=as.character(phd_year)),
  read_csv("data/survey_data/survey_sdc.csv") |>
    mutate(distribution="SDC", phd_year=as.character(phd_year)),
  read_csv("data/survey_data/survey_psych.csv") |>
    mutate(distribution="Psych", phd_year=as.character(phd_year)),
)
table(df$distribution)
nrow(df)

#Processing
df = df |>
  mutate(
    ai_good = c("Very negative"=1, "Somewhat negative"=2, "Equally positive and negative"=3, "Somewhat positive"=4, "Very positive"=5)[ai_positive_effect],
    llm_use_research = c("Never"=1, "1 time per month or less"=2, "2-3 times per month"=3, "1 time per week"=4, "2-3 times per week"=5, "1 time per day"=6, "2+ times per day"=7)[llm_use_1],
    llm_use_nonresearch = c("Never"=1, "1 time per month or less"=2, "2-3 times per month"=3, "1 time per week"=4, "2-3 times per week"=5, "1 time per day"=6, "2+ times per day"=7)[llm_use_2],
    exp_survey_exp = c("No experience at all1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5, "6"= 6, "A great deal of experience7" = 7)[exp_survey_exp],
    exp_lab_exp = c("No experience at all1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5, "6"= 6, "A great deal of experience7" = 7)[exp_lab_exp],
    exp_ml_ai = c("No experience at all1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5, "6"= 6, "A great deal of experience7" = 7)[exp_ml_ai],
    survey_exp_yn = c("Yes" = 1, "No" = 2)[survey_exp_yn],
    polit = c("Extremely Liberal" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5, "6"= 6, "Extremely conservative" = 7)[polit])

#Demographics/descriptives
#Gender
100*table(df$Gender)/nrow(df[!is.na(df$Gender),])

#race
100*table(df$race)/nrow(df[!is.na(df$race),])
nrow(df[!is.na(df$race) & df$race!="White / Caucasian",])/nrow(df[!is.na(df$race),])

#survey_exp_yn
table(df$survey_exp_yn)/nrow(df[!is.na(df$survey_exp_yn),])

# Disciplines
df |>
  select(discipline) |>
  separate_rows(discipline, sep=",") |>
  group_by(discipline) |>
  dplyr::summarize(perc =100*n() / nrow(df)) |>
  arrange(-perc)#|>
#mutate(discipline = fct_reorder(discipline, perc))# |>
#ggplot(aes(y=discipline, x=proportion)) |>
# geom_col(stat="identity") +
# theme_bw() + theme(
#   panel.grid.major.y = element_blank(),
#   axis.title.y = element_blank()
# ) +
# ggtitle("Disciplines")
#g1
#ggsave("~/Dropbox/Stanford/GPT/Survey/plots/disciplines.png", g1, width=8, height=5)

# Position
df |>
  group_by(position) |>
  dplyr::summarize(perc = 100 * n() / nrow(df)) |>
  mutate(position = fct_reorder(position, perc)) |>
  arrange(-perc)#|>
# ggplot(aes(y=position, x=proportion)) +
# geom_col(stat="identity") +
# theme_bw() + theme(
#   panel.grid.major.y = element_blank(),
#   axis.title.y = element_blank()
# ) +
# ggtitle("Positions")
#g2
#ggsave("~/Dropbox/Stanford/GPT/Survey/plots/positions.png", g2, width=8, height=5)


# Which applications are scientists most likely to use?
uses_df = df |>
  select(matches("^use_.*_[15]$")) |>
  pivot_longer(everything()) |>
  mutate(name = str_match(name, "^use_(.*)_[15]$")[,2]) |>
  group_by(name) |>
  group_modify(function(d,k) {
    lm(value~1, d) |> broom::tidy(conf.int=T)
  }) |>
  ungroup() |>
  mutate(highlight = if_else(name == "overall", "highlight", "normal"),
         label = c(
                        "overall" = "<b>Overall, for any purpose</b>",
                        "power_analysis" = "Power analysis",
                        "interventions" = "Identifying promising interventions",
                        "pilots" = "Running AI-based pilot studies",
                        "replication" = "Identifying published effects that need replication",
                        "mediators" = "Identifying potential mediators",
                        "moderators" = "Identifying potential moderators",
                        "generalizability" = "Assessing generalizability to alternate stimuli versions",
                        "harm" = "Simulating studies involving harm exposure",
                        "hard_to_reach" = "Assessing generalizability to hard-to-reach samples",
                        "combine" = "Combining w/ human samples for greater accuracy")[name]) |>
  mutate(label = label |> fct_reorder(estimate) |> fct_relevel("<b>Overall, for any purpose</b>"))
uses_df

#plotting
uses_graph <- uses_df|>
  ggplot(aes(y=label, x=estimate, xmin=conf.low, xmax=conf.high)) +
  geom_vline(xintercept = 50, linetype="dashed", alpha=0.5)+
  geom_col(aes(alpha = highlight), fill=scales::hue_pal()(3)[[2]])+
  scale_alpha_manual(values = c("highlight" = 0.85, "normal" = 0.55)) +
  theme(axis.text.y = element_blank()) +guides(fill = "none", color = "none")+
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2)+
  theme_bw() + theme(
    axis.text.y = ggtext::element_markdown(size=8),
    panel.grid.major = element_blank(),
    legend.position = "none",
    panel.grid.minor = element_blank(), axis.title.y = element_blank(),
    ) +
  coord_cartesian(xlim=c(0,100), clip = "off")+
  xlab("Likelihood of use")
uses_graph
saveRDS(uses_graph, "output/processed_data/survey_uses_graph.rds")


#get mean and sd
uses_msd = df |>
  select(matches("^use_.*_[15]$")) |>
  pivot_longer(everything()) |>
  mutate(name = str_match(name, "^use_(.*)_[15]$")[,2]) |>
  group_by(name) |>
  dplyr::summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    .groups = "drop")
uses_msd

#N of people who selected a number above 50% for at least one application
df_max_use <- df |>
  select(matches("^use_.*_[15]$")) |>
  dplyr::mutate(row_id = row_number()) |>
  pivot_longer(
    cols = -row_id, names_to = "variable", values_to = "value"
  ) |>
  group_by(row_id) |>
  slice_max(order_by = value, n = 1, with_ties = FALSE) |>
  ungroup() |>
  filter(value > 50) %>%
  nrow()
df_max_use
df_max_use/nrow(df)

# What are scientists most concerned about?
concerns_df = df |>
  select(matches("^concern_.*_[15]$")) |>
  pivot_longer(everything()) |>
  mutate(name = str_match(name, "^concern_(.*)_[15]$")[,2]) |>
  group_by(name) |>
  group_modify(function(d,k) {
    lm(value~1, d) |> broom::tidy(conf.int=T)
  }) |>
  ungroup() |>
  mutate(name = name |> fct_reorder(-estimate) |> fct_rev()) |>
  mutate(label = recode(name,
                        "stereotypes" = "Bias or alignment with stereotypes",
                        "variance" = "Underestimation of variance",
                        "minority_acc" = "Low accuracy for underrepresented groups",
                        "proprietary" = "Low reproducibility because of proprietary models",
                        "replacing" = "AI responses replacing human samples",
                        "black_box" = "Low interpretability because of black-box models",
                        "focus" = "Shift away from research areas less predictable by AI",
                        "misuse" = "Misuse for exploitative or harmful purposeses",
                        "false_negati" = "Overlooking promising ideas due to false negatives"))
concerns_df
#plot
concerns_df %>% summarize(m = mean(estimate), sd = sd(estimate))

concerns_graph <- concerns_df |>
  ggplot(aes(y=label, x=estimate, xmin=conf.low, xmax=conf.high)) +
  geom_vline(xintercept = 50, color = "gray40", alpha =.2)+
  guides(fill="none")+
  geom_col(fill = scales::hue_pal()(3)[[1]], alpha=0.7)+
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2)+
  theme_bw() + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), axis.title.y = element_blank()) +
  coord_cartesian(xlim=c(0,100)) +
  xlab("Level of concern")
concerns_graph
ggsave("output/plots/concerns.png", concerns_graph, width=7, height=3)

#get mean and sd
concerns_msd = df |>
  select(matches("^concern_.*_[15]$")) |>
  pivot_longer(everything()) |>
  mutate(name = str_match(name, "^concern_(.*)_[15]$")[,2]) |>
  group_by(name) |>
  dplyr::summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    .groups = "drop")
concerns_msd

#Does likelihood of use vary by discipline?
# Only 4 people selected public health; 
# Only 1 person selected public health alone; so we consider them "Other"
df_disciplines <- df |>
  mutate(discipline = ifelse(!is.na(discipline) & discipline=="Public Health", "Other (please specify)", discipline))|>
  separate_rows(discipline, sep = ",") |>
  filter(!is.na(discipline)) |>
  filter(discipline!="Public Health") |>
  mutate(discipline = factor(ifelse(!is.na(discipline) & discipline=="Other (please specify)", "Other", discipline)))

table(df_disciplines$discipline)

#Differences in overall likelihood of use across disciplines
contrasts(df_disciplines$discipline) <- contr.sum
summary(lm(use_overall_5 ~ discipline, df_disciplines)) #no difference, but each person selected multiple disciplines
summary(lmer(use_overall_5 ~ discipline + (1 | ResponseId), df_disciplines)) #convergence issues

#selecting 1 random discipline per response
results_df <- map_dfr(1:50, function(i) {
  sampled_df <- df_disciplines %>%
    group_by(ResponseId) %>%
    slice_sample(n = 1) %>%
    ungroup()
  contrasts(sampled_df$discipline) <- contr.sum
  model <- lm(use_overall_5 ~ discipline, data = sampled_df)
  tidy(model) %>% mutate(iteration = i)
})
results_df %>% group_by(term) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  mutate(p.value = format(p.value, scientific = FALSE))

#Differences in application-specific likelihood of use across disciplines
uses_discipline_ci <- df_disciplines |>
  select(discipline, names(df |> select(matches("^use_.*_[15]$")))) |>
  pivot_longer(cols = -discipline, names_to = "use", values_to = "value") |>
  Rmisc::summarySE(measurevar = "value",
                   groupvars = c("discipline", "use"),
                   na.rm = TRUE) |>
  mutate(use = str_match(use, "^use_(.*)_[15]$")[,2]) |>
  mutate(use = use |> fct_reorder(-value) |> fct_relevel("overall") |> fct_rev()) |>
  mutate(discipline = discipline |> fct_relevel("Psychology", "Political Science", "Sociology", "Economics", "Other")) |>
  mutate(label = recode(use,
                        "overall" = "Overall, for any purpose",
                        "power_analysis" = "Power analysis",
                        "interventions" = "Identifying promising interventions",
                        "pilots" = "Running AI-based pilot studies",
                        "replication" = "Identifying published effects that need replication",
                        "mediators" = "Identifying potential mediators",
                        "moderators" = "Identifying potential moderators",
                        "generalizability" = "Assessing generalizability to alternate stimuli versions",
                        "harm" = "Simulating studies involving harm exposure",
                        "hard_to_reach" = "Assessing generalizability to hard-to-reach samples",
                        "combine" = "Combining with human samples for greater accuracy")) 

#plot
g_use_bydiscipline <- ggplot(uses_discipline_ci, aes(y = label, x = value)) +
  facet_wrap(~discipline) +
  geom_vline(xintercept = 50, color = "gray40", alpha =.2)+
  geom_col(fill = scales::hue_pal()(3)[[2]], width = .7, alpha=0.6)+
  geom_errorbarh(aes(xmin = value - ci, xmax = value + ci), height = 0.2)+
  labs(
    x = "Likelihood of use",
    y = ""
  ) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
g_use_bydiscipline
ggsave("output/plots/use_by_disciplines.png", g_use_bydiscipline, width=8, height=5)

#Does concern about risks vary by discipline?
concerns_discipline_ci <- df_disciplines |>
  select(discipline, names(df |> select(matches("^concern_(.*)_[15]$")))) |>
  pivot_longer(cols = -discipline, names_to = "concern", values_to = "value") |>
  Rmisc::summarySE(measurevar = "value",
                   groupvars = c("discipline", "concern"),
                   na.rm = TRUE) |>
  mutate(concern = str_match(concern, "^concern_(.*)_5$")[,2]) |>
  mutate(concern = concern |> fct_reorder(-value) |> fct_rev()) |>
  mutate(discipline = discipline |> fct_relevel("Psychology", "Political Science", "Sociology", "Economics", "Other")) |>
  mutate(label = recode(concern,
                        "stereotypes" = "Bias or alignment with stereotypes",
                        "variance" = "Underestimation of variance",
                        "minority_acc" = "Low accuracy for underrepresented groups",
                        "proprietary" = "Low reproducibility because of proprietary models",
                        "replacing" = "AI responses replacing human samples",
                        "black_box" = "Low interpretability because of black-box models",
                        "focus" = "Shift away from research areas less predictable by AI",
                        "misuse" = "Misuse for exploitative or harmful purposeses",
                        "false_negati" = "Overlooking promising ideas due to false negatives"))

#plot
g_concern_bydiscipline <- ggplot(concerns_discipline_ci, aes(y = label, x = value)) +
  facet_wrap(~discipline) +
  geom_vline(xintercept = 50, color = "gray40", alpha =.2)+
  geom_col(fill = scales::hue_pal()(3)[[1]], width = .7, alpha=0.6)+
  geom_errorbarh(aes(xmin = value - ci, xmax = value + ci), height = 0.2)+
  labs(
    x = "Concern",
    y = ""
  ) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

g_concern_bydiscipline
ggsave("output/plots/concern_by_disciplines.png", g_concern_bydiscipline, width=8, height=5)


#Does likelihood of use vary by career stage?
table(df$position)
df_positions <- df |>
  filter(!is.na(position))|>
  mutate(position = case_when(position == "Assistant Professor, or equivalent" ~ "Assistant Professor",
                              position == "Associate Professor, or equivalent" ~ "Associate Professor",
                              position == "Professor, or equivalent" ~ "Professor",
                              position == "Postdoctoral fellow or researcher" ~ "Postdoctoral fellow",
                              position == "Graduate student" ~ "Graduate student",
                              TRUE ~ "Other")) |>
  mutate(position = position |> fct_relevel("Professor", "Associate Professor", "Assistant Professor", "Postdoctoral fellow","Graduate student" , "Other"))
table(df_positions$position)

contrasts(df_positions$position) <- contr.sum
anova(lm(use_overall_5 ~ position, df_positions)) #no difference, but each 

uses_position_ci <- df_positions|>
  select(position, names(df |> select(matches("^use_.*_[15]$")))) |>
  pivot_longer(cols = -position, names_to = "use", values_to = "value") |>
  Rmisc::summarySE(measurevar = "value",
                   groupvars = c("position", "use"),
                   na.rm = TRUE) |>
  mutate(use = str_match(use, "^use_(.*)_[15]$")[,2]) |>
  mutate(use = use |> fct_reorder(-value) |> fct_relevel("overall") |> fct_rev()) |>
  mutate(label = recode(use,
                        "overall" = "Overall, for any purpose",
                        "power_analysis" = "Power analysis",
                        "interventions" = "Identifying promising interventions",
                        "pilots" = "Running AI-based pilot studies",
                        "replication" = "Identifying published effects that need replication",
                        "mediators" = "Identifying potential mediators",
                        "moderators" = "Identifying potential moderators",
                        "generalizability" = "Assessing generalizability to alternate stimuli versions",
                        "harm" = "Simulating studies involving harm exposure",
                        "hard_to_reach" = "Assessing generalizability to hard-to-reach samples",
                        "combine" = "Combining with human samples for greater accuracy")) 

#plot
g_use_byposition <- ggplot(uses_position_ci, aes(y = label, x = value)) +
  facet_wrap(~position) +
  geom_vline(xintercept = 50, color = "gray40", alpha =.2)+
  geom_col(fill = scales::hue_pal()(3)[[2]], width = .7, alpha=0.6)+
  geom_errorbarh(aes(xmin = value - ci, xmax = value + ci), height = 0.2)+
  labs(
    x = "Likelihood of use",
    y = ""
  ) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
g_use_byposition
ggsave("output/plots/use_by_position.png", g_use_byposition, width=8, height=5)

#Does concerns about risks vary by career stage?
concerns_position_ci <- df_positions |>
  select(position, names(df |> select(matches("^concern_(.*)_[15]$")))) |>
  pivot_longer(cols = -position, names_to = "concern", values_to = "value") |>
  Rmisc::summarySE(measurevar = "value",
                   groupvars = c("position", "concern"),
                   na.rm = TRUE) |>
  mutate(concern = str_match(concern, "^concern_(.*)_5$")[,2]) |>
  mutate(concern = concern |> fct_reorder(-value) |> fct_rev()) |>
  mutate(label = recode(concern,
                        "stereotypes" = "Bias or alignment with stereotypes",
                        "variance" = "Underestimation of variance",
                        "minority_acc" = "Low accuracy for underrepresented groups",
                        "proprietary" = "Low reproducibility because of proprietary models",
                        "replacing" = "AI responses replacing human samples",
                        "black_box" = "Low interpretability because of black-box models",
                        "focus" = "Shift away from research areas less predictable by AI",
                        "misuse" = "Misuse for exploitative or harmful purposeses",
                        "false_negati" = "Overlooking promising ideas due to false negatives"))

#plot
g_concern_byposition <- ggplot(concerns_position_ci, aes(y = label, x = value)) +
  facet_wrap(~position) +
  geom_vline(xintercept = 50, color = "gray40", alpha =.2)+
  geom_col(fill = scales::hue_pal()(3)[[1]], width = .7, alpha=0.6)+
  geom_errorbarh(aes(xmin = value - ci, xmax = value + ci), height = 0.2)+
  labs(
    x = "Concern",
    y = ""
  ) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

g_concern_byposition
ggsave("output/plots/concern_by_positions.png", g_concern_byposition, width=8, height=5)

#Other predictors
cor_df <- df |>
  select(gender = Gender, race, polit,
         llm_use_research, llm_use_nonresearch, ai_good, 
         exp_survey_exp, exp_lab_exp, exp_ml_ai, 
         matches("^use_.*_[15]$"), #all uses
         matches("^concern_.*_[15]$", #all concerns
         )) %>%
  dplyr::rename("overall_use" = "use_overall_5") %>%
  mutate(mean_use = rowMeans(select(., matches("^use_.*_[15]$")), na.rm = TRUE),
         mean_concern = rowMeans(select(., matches("^concern_.*_[15]$")), na.rm = TRUE),
         female = c("Female"=1, "Male"=0)[gender],
         nonWhite = c("White / Caucasian"=0, 
                      "Asian / Asian-American"=1, "Black / African-American" = 1, 
                      "Latino / Hispanic" = 1, "Other" = 1)[race],
         llm_use = rowMeans(select(., llm_use_research, llm_use_nonresearch), na.rm=T)) %>%
  select("Overall likelihood of use" = overall_use, 
         "Avg. likelihood of use" = mean_use, 
         "Avg. concern" = mean_concern,
         "LLM use frequency" = llm_use, 
         "AI optimistism" = ai_good, 
         "Experience with survey expts" = exp_survey_exp, 
         "Experience with lab expts" = exp_lab_exp, 
         "Experience with ML/AI" = exp_ml_ai,
         "Female" = female, 
         "Non-White" = nonWhite, 
         "Conservative" = polit
  )

# correl1 <- cor_df %>%
#  corrr::correlate() %>%
#   shave %>%
#   flextable() %>%
#   save_as_docx(path = "~/Dropbox/Stanford/GPT/Survey/plots/correlation_table.docx")

# apa.cor.table(cor_df, filename = "~/Dropbox/Stanford/GPT/Survey/plots/cor_table.doc", show.conf.interval = FALSE)

#plotting a heatmap
#correlations <- cor_df %>%
# cor(use = "pairwise.complete.obs")
#correlations[upper.tri(correlations)] <- NA 

correlations <- Hmisc::rcorr(as.matrix(cor_df), type = "pearson")
cor_mat <- correlations$r       # correlation coefficients
p_mat   <- correlations$P       # p-values
cor_mat[upper.tri(cor_mat)] <- NA
p_mat[upper.tri(p_mat)] <- NA
cor_long <- reshape::melt(cor_mat, na.rm = FALSE)
p_long   <- reshape::melt(p_mat, na.rm = FALSE)

full_df <- cor_long %>%
  dplyr::rename(cor = value) %>%
  left_join(p_long %>% dplyr::rename(p = value), by = c("X1", "X2")) %>%
  mutate(
    sig = case_when(
      is.na(p) ~ "",
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE ~ ""
    ),
    label = ifelse(is.na(cor), "", paste0(sprintf("%.2f", cor), sig)),
    X1 = factor(X1, levels = rev(colnames(cor_mat))),
    X2 = factor(X2, levels = colnames(cor_mat))
  )

g_heatmap <- ggplot(full_df, aes(X2, X1, fill = cor)) +
  geom_tile(color = "white") +
  geom_text(aes(label = label), color = "black", size = 2) +
  scale_fill_gradient2(
    low = "#4575b4", mid = "#f0f0f0", high = "#d73027",
    midpoint = 0, limit = c(-1, 1), name = "Correlation",
    na.value = "white"
  ) +
  scale_x_discrete(position = "top") +
  theme_bw() +
  theme(
    axis.text.x = element_text(color = "black", size = 10, angle = 45,
                               vjust = 0.5, hjust = 0, margin = margin(b = 10)),
    axis.text.y = element_text(color = "black", size = 10),
    plot.margin = margin(t = 40, r = 20, b = 20, l = 20),
    axis.title = element_blank()
  ) +
  labs(x = "", y = "", title = "")

# g_heatmap <- correlations %>%
#   reshape::melt(na.rm=T) %>%
#   mutate(X1 = factor(X1, levels = rev(colnames(correlations))),
#          X2 = factor(X2, levels = colnames(correlations))) %>%
#   ggplot(aes(X2, X1, fill = value)) +
#   geom_tile() +
#   geom_text(aes(label = round(value, 2)), color = "black", size = 2.5) +
#   scale_fill_gradient2(
#     low = "#4575b4", mid = "#f0f0f0", high = "#d73027",  
#     midpoint = 0, limit = c(-1, 1),
#     na.value = "gray95"  # for masked cells
#   ) +  theme_bw() +
#   theme(axis.text.x = element_text(color = "black", size = 10, angle = 45,
#     vjust = 0.5, hjust = 0, margin = margin(b = 10)),
#   axis.text.y = element_text(color = "black", size = 10),
#   plot.margin = margin(t = 40, r = 20, b = 20, l = 20),
#   axis.title = element_blank()) +
#   labs(x = "", y = "", title = "")+
#   scale_x_discrete(position = "top") 

g_heatmap
ggsave("output/plots/g_heatmap.png", g_heatmap, width=7, height=5)

