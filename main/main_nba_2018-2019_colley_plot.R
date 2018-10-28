library(jsonlite)
library(zoo)
library(tidyverse)
library(magrittr)
library(rio)

setwd("C:/Users/Wenyao/Desktop/R/Who-is-number-1")
source("./functions/functions_scrape_nba.R")
source("./functions/functions_general.R")
source("./functions/functions_colley's_method.R")


#==== 2018 - 2019 Season ====
regular_season_2018_2019 <- scrape_nba_scoreboard(
  start_date = "2018-10-16",
  end_date = Sys.Date() %>% as.character()
) %>% 
  # remove future games
  na.omit()


#==== feed the match results incrementaly to the ranking algorithm
# the first ranking can be produced after the 2nd day
# when all teams have at least had one game
freqeuncy <- 1
as_of_dates <- seq(from = as.Date("2018-10-18"), to = Sys.Date(), by = freqeuncy)

# for each training window, find the Colley ranking
rankings <- as_of_dates %>% 
  map(function(as_of_date){
    tibble(
      rank = 1:30,
      team = colleys_method(
        scoreboard = regular_season_2018_2019 %>% 
          filter(as.Date(date, "%Y%m%d") <= as_of_date)
      ) %>% 
        format_ratings()
    )
  }) %>% 
  set_names(as_of_dates) %>% 
  bind_rows(.id = "as_of_date") %>% 
  mutate(
    as_of_date = as.Date(as_of_date),
    day = as.numeric(as_of_date - as.Date("2018-10-16") + 1)
  )

# load the NBA color palette
nba_color_palette <- import("data/NBA_Color_Palette.csv")

plot <- ggplot(data = rankings, aes(x = day, y = rank, group = team)) +
  geom_line(aes(alpha = 1, color = team), size = 2) +
  geom_point(aes(alpha = 1, color = team), size = 6) +
  geom_point(color = "white", size = 2) +
  scale_x_continuous(
    breaks = seq(from = min(rankings$day), to = max(rankings$day), by = freqeuncy), 
    minor_breaks = seq(from = min(rankings$day), to = max(rankings$day), by = freqeuncy), 
    expand = c(.05, .05),
    labels = as_of_dates %>% format("%b %d")
  ) +
  scale_y_reverse(breaks = 1:30, sec.axis = dup_axis()) +
  # the label background box on the left side
  geom_tile(
    data = rankings %>% filter(day == min(rankings$day)), 
    aes(x = min(rankings$day) - 0.6, y = rank, fill = team, color = team),
    height = 0.6, 
    width = 0.6,
    size = 1.3
  ) + 
  # the label background box on the right side
  geom_tile(
    data = rankings %>% filter(day == max(day)), 
    aes(x = max(rankings$day) + 0.6, y = rank, fill = team, color = team),
    height = 0.6,
    width = 0.6,
    size = 1.3
  ) + 
  # the label on the left side
  geom_text(
    data = rankings %>% filter(day == min(day)),
    aes(label = team, x = min(rankings$day) - 0.6) , 
    fontface = "bold", 
    color = "white", 
    size = 5
  ) +
  # the label on the right side
  geom_text(
    data = rankings %>% filter(day == max(day)),
    aes(label = team, x = max(rankings$day) + 0.6) ,
    fontface = "bold",
    color = "white",
    size = 5
  ) +
  scale_fill_manual(
    values = nba_color_palette$major_color %>% set_names(nba_color_palette$team_short_name)
  ) +
  scale_color_manual(
    values = nba_color_palette$minor_color %>% set_names(nba_color_palette$team_short_name)
  ) +
  labs(
    x = "",
    y = "",
    title = "NBA Power Ranking - Colley's Method",
    subtitle = paste0("Last updated on ", Sys.Date() %>% format("%b %d, %Y"))
  ) +
  theme_bw(base_size = 20) +
  theme(
    legend.position = "none",
    plot.margin = margin(0.5, -0.5, 0, -0.5, "cm"),
    axis.text.y.left = element_text(margin = margin(0, -1, 0, 0, "cm")),
    axis.text.y.right = element_text(margin = margin(0, 0, 0, -1, "cm")),
    axis.ticks = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank()
  )

print(plot)


#==== output =====
svg("output/NBA_Ranking_2018-2019.svg", width = 3 * 4, height = 5 * 4)
print(plot)
dev.off()

png("output/NBA_Ranking_2018-2019.png", width = 880, height = 1500)
print(plot)
dev.off()
