# 03_anova_model.R
# 06 - 11 - 21

# libraries
library(tidyverse)
library(tidymodels)

# read in data
judge_aspect_scores <-
  read_csv(here::here("data/judge_aspect_scores.csv"))
judge_performance_scores <-
  read_csv(here::here("data/judge_performance_scores.csv"))
judges <- read_csv(here::here("data/judges.csv"))

# set up anova data frame (or KW) - by judges
#' TODO: otherize countries that don't have
#' corresponding judges

anova_df <- judge_performance_scores |>
  left_join(judges, by = c("program", "judge" = "clean_role")) |>
  select(clean_judge_name, judge_country, program, nation, final_score) |>
  mutate(
    clean_judge_name = parse_factor(clean_judge_name),
    program = parse_factor(program),
    country_match = case_when(
judge_country == nation ~ "SAME",
TRUE ~ "DIFF"),
country_match = parse_factor(country_match),
    judge_country = case_when(
      str_detect(judge_country, "AUS") ~ "AUS",
      str_detect(judge_country, "BEL") ~ "BEL",
      str_detect(judge_country, "CAN") ~ "CAN",
      str_detect(judge_country, "CHN") ~ "CHN",
      str_detect(judge_country, "CZE") ~ "CZE",
      str_detect(judge_country, "ESP") ~ "ESP",
      str_detect(judge_country, "FRA") ~ "FRA",
      str_detect(judge_country, "GER") ~ "GER",
      str_detect(judge_country, "HUN") ~ "HUN",
      str_detect(judge_country, "ISR") ~ "ISR",
      str_detect(judge_country, "ITA") ~ "ISA",
      str_detect(judge_country, "JPN") ~ "JPN",
      str_detect(judge_country, "KAZ") ~ "KAZ",
      str_detect(judge_country, "KOR") ~ "KOR",
      str_detect(judge_country, "LAT") ~ "LAT",
      str_detect(judge_country, "OAR") ~ "OAR",
      str_detect(judge_country, "POL") ~ "POL",
      str_detect(judge_country, "SVK") ~ "SVK",
      str_detect(judge_country, "TUR") ~ "TUR",
      str_detect(judge_country, "UKR") ~ "UKR",
      TRUE ~ "OTHER"
      ),
    judge_country = parse_factor(judge_country),
    nation = case_when(
      str_detect(nation, "AUS") ~ "AUS",
      str_detect(nation, "BEL") ~ "BEL",
      str_detect(nation, "CAN") ~ "CAN",
      str_detect(nation, "CHN") ~ "CHN",
      str_detect(nation, "CZE") ~ "CZE",
      str_detect(nation, "ESP") ~ "ESP",
      str_detect(nation, "FRA") ~ "FRA",
      str_detect(nation, "GER") ~ "GER",
      str_detect(nation, "HUN") ~ "HUN",
      str_detect(nation, "ISR") ~ "ISR",
      str_detect(nation, "ITA") ~ "ISA",
      str_detect(nation, "JPN") ~ "JPN",
      str_detect(nation, "KAZ") ~ "KAZ",
      str_detect(nation, "KOR") ~ "KOR",
      str_detect(nation, "LAT") ~ "LAT",
      str_detect(nation, "OAR") ~ "OAR",
      str_detect(nation, "POL") ~ "POL",
      str_detect(nation, "SVK") ~ "SVK",
      str_detect(nation, "TUR") ~ "TUR",
      str_detect(nation, "UKR") ~ "UKR",
      TRUE ~ "OTHER"
    ),
    nation = parse_factor(nation)
  )

# create lm model - separate model for event
lm_models <- anova_df |>
  nest(-c(program)) |>
  mutate(
    fit = map(data,
              ~ lm(final_score ~ judge_country + nation + country_match,
                   data = .)),
    results = map(fit, tidy),
    R_squared = map(fit, ~ summary(.x)$adj.r.squared),
  ) |>
  unnest_longer(R_squared) |>
  unnest(results)

# plot of R squared values
lm_models |>
  select(program, R_squared) |>
  group_by(program) |>
  summarize(R_squared = round(abs(mean(R_squared)), 2)) |>
  ggplot(aes(x = program, y = R_squared)) +
  geom_point()

# send diagnostic plots to output folder
save_diag_plots <- function(num) {
  png(paste0(here::here("output/diagnostic_plots/"),
             "model_",formatC(num, flag = "0",digits = 1),
             "_diagnostics.png", sep = ""))
  par(mfrow = c(2,2))
  plot(lm_models$fit[[num]])
  par(mfrow = c(1,1))
  dev.off()
}

map(1:16,save_diag_plots)

# TODO: interaction plots
