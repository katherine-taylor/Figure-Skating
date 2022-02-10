# 01_data_pull.R
# 06-09-21

# libraries
library(tidyverse)

# import data from github
judge_goe <- read_csv("https://raw.githubusercontent.com/BuzzFeedNews/2018-02-olympic-figure-skating-analysis/master/data/judge-goe.csv")
judge_scores <- read_csv("https://raw.githubusercontent.com/BuzzFeedNews/2018-02-olympic-figure-skating-analysis/master/data/judge-scores.csv")
judged_aspects <- read_csv("https://raw.githubusercontent.com/BuzzFeedNews/2018-02-olympic-figure-skating-analysis/master/data/judged-aspects.csv")
judges <- read_csv("https://raw.githubusercontent.com/BuzzFeedNews/2018-02-olympic-figure-skating-analysis/master/data/judges.csv")
performances <- read_csv("https://raw.githubusercontent.com/BuzzFeedNews/2018-02-olympic-figure-skating-analysis/master/data/performances.csv")

# look at data
map(list(judge_goe, judge_scores, judged_aspects, judges, performances),glimpse)

# calculate my own scores
judge_aspect_scores <- judged_aspects |>
  left_join(judge_goe, by = "aspect_id") |>
  left_join(judge_scores, by = c("aspect_id","judge")) |>
  filter(!is.na(judge)) |>
  group_by(aspect_id, performance_id, aspect_desc, section, judge) |>
  summarize(total_score = case_when(
      section == "elements" ~ base_value + judge_goe,
      section == "components" ~ factor * score
    )
  ) |>
  ungroup()

judge_performance_scores <- judge_aspect_scores |>
  group_by(performance_id, judge) |>
  summarise(total_score = sum(total_score, na.rm = TRUE)) |>
  left_join(performances, by = "performance_id") |>
  select(name, performance_id, nation, program, judge, total_score, total_deductions) |>
  mutate(final_score = total_score + total_deductions)

write_csv(judge_aspect_scores, here::here("data/judge_aspect_scores.csv"))
write_csv(judge_performance_scores, here::here("data/judge_performance_scores.csv"))
write_csv(judges, here::here("data/judges.csv"))
  