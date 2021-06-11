# 02_quad_jump_vis.R
# 06 - 10 - 21

# libraries
library(tidyverse)
# install.packages("ggdist")
library(ggdist)
# install.packages("ghibli")
library(ghibli)
# install.packages("gghalves")
library(gghalves)
# install.packages("systemfonts")
library(systemfonts)

# constants
SEED <- 3171998

# import data
judge_aspect_scores <- read_csv(here::here("data/judge_aspect_scores.csv"))
judge_performance_scores <- read_csv(here::here("data/judge_performance_scores.csv"))
judges <- read_csv(here::here("data/judges.csv"))

# plot scoring of quads for funsises
glimpse(judge_aspect_scores)

judge_aspect_scores |>
  filter(
    str_detect(aspect_desc, "4T"),!str_detect(aspect_desc, "<<"),!str_detect(aspect_desc, "COMBO")
  ) |>
  ggplot(aes(
    x = aspect_desc,
    y = total_score,
    fill = aspect_desc,
    color = aspect_desc
  )) +
  stat_halfeye(point_color = NA,
               justification = -0.3,
               width = 0.6) +
  geom_boxplot(width = 0.15,
               outlier.shape = NA) +
  geom_half_point(side = "l",
                  range_scale = 0.4,
                  alpha = 0.5) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        text = element_text(family = "Arial"),
        plot.title = element_text(face = "bold", size = 16)) +
  scale_fill_ghibli_d("PonyoLight") +
  scale_color_ghibli_d("PonyoMedium") +
  labs(
    title = "Quad Jump Scores",
    subtitle = "2018 Winter Olympics - Figure Skating",
    x = "Type of Jump",
    y = "Score",
    caption = "Data from BuzzFeed News"
  )

ggsave(here::here("figures/quad_jumps_1.png"))

  