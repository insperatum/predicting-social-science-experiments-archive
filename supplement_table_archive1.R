library(kableExtra)
library(knitr)
library(kableExtra)
knitr::opts_chunk$set(echo = TRUE)
escape_latex <- function(x) {
  specials <- c("\\", "%", "$", "#", "_", "{", "}", "~", "^", "&")
                escaped <- x
                for (char in specials) {
                  escaped <- gsub(char, paste0("\\", char), escaped, fixed = TRUE)
  }
  # Special handling for tilde and hat characters
  escaped <- gsub("~", "\\textasciitilde{}", escaped, fixed = TRUE)
  escaped <- gsub("\\^", "\\textasciicircum{}", escaped, fixed = TRUE)
  return(escaped)
}


d = df_experiments %>%
  filter(is.na(hypothesis), participants=="all") %>%
  group_by(study) %>%
  summarize(
    N_outcomes = n_distinct(outcome.name),
    N_participants = max(N_regression),
    N_conditions = n_distinct(reference_condition),
  ) %>%
  ungroup() %>%
  left_join(study_features) %>%
  mutate(
    title=study_title,
    authors=study_authors,
    field=study_field
  )

df_table = d %>%
  arrange(-N_participants) %>%
  transmute(Study = str_glue("{row_number()}. \\textbf{{{escape_latex(title)}}} \\textit{{({escape_latex(authors)})}}"),
            Field=escape_latex(field),
            `N conditions`=N_conditions,
            `N outcomes`=N_outcomes,
            # N effects=N_effects.rct,
            `N subjects`=prettyNum(N_participants, big.mark=",")
  ) %>%
  rename_with(\(s) ifelse(s %in% c("Study", "Field"), s, paste0("\\rotatebox{90}{", s, "}")))
df_table %>%
  kable("latex", booktabs = TRUE, longtable = TRUE, escape = FALSE, linesep = "\\addlinespace") %>%
  kable_styling(latex_options = "hold_position") %>%
  column_spec(1, width = "11cm") %>%
  column_spec(2, width = "5cm") %>%
  kable_styling(latex_options = "repeat_header") %>%
  landscape()
