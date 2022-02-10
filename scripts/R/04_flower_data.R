# 04_flower_data.R
# 2022-02-09


# Libraries ---------------------------------------------------------------

library(tidyverse)


# Load Data ---------------------------------------------------------------

judge_aspect_scores <- read_csv(here::here("data/judge_aspect_scores.csv"))
judge_performance_scores <- read_csv(here::here("data/judge_performance_scores.csv"))
judges <- read_csv(here::here("data/judges.csv"))

