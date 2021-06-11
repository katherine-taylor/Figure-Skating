# 03_anova_model.R
# 06 - 11 - 21

# libraries
library(tidyverse)
library(broom)

# read in data
judge_aspect_scores <- read_csv(here::here("data/judge_aspect_scores.csv"))
judge_performance_scores <- read_csv(here::here("data/judge_performance_scores.csv"))
judges <- read_csv(here::here("data/judges.csv"))

# set up anova data frame (or KW) - by judges
#' TODO: otherize countries that don't have
#' corresponding judges

anova_df <- judge_performance_scores |> 
  left_join(judges, by = c("program","judge" = "clean_role")) |>
  select(clean_judge_name, judge_country, program, nation, final_score) |>
  mutate(clean_judge_name = parse_factor(clean_judge_name),
         judge_country = parse_factor(judge_country),
         program = parse_factor(program),
         nation = parse_factor(nation))

# create lm model - separate model for event
lm_models <- anova_df |>
  nest(-c(program))|>
  mutate(fit = map(data,
                   ~ lm(final_score ~ judge_country * nation,
                        data = .)),
         results = map(fit, tidy),
         R_squared = map(fit, ~ summary(.x)$adj.r.squared),
         ) |>
  select(program, R_squared, results) |>
  unnest_longer(R_squared) |>
  unnest(results)
  
# unnest results somehow

# plot of R squared values
lm_models |>
  select(program, R_squared) |>
  group_by(program) |>
  summarize(R_squared = round(abs(mean(R_squared)),2)) |>
  ggplot(aes(x = program, y = R_squared)) +
  geom_point()

# TODO: send diagnostic plots to folder
# TODO: interaction plots


# set up anova data frame - by element