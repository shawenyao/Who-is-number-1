suppressWarnings(library(jsonlite))
suppressWarnings(library(zoo))
suppressWarnings(library(tidyverse))
suppressWarnings(library(magrittr))
suppressWarnings(library(rio))
suppressWarnings(library(beepr))


#==== general setup ====
setwd("C:/Users/Wenyao/Desktop/R/Who-is-number-1")
source("./functions/functions_scrape_nba.R")
source("./functions/functions_general.R")
source("./functions/functions_colley's_method.R")
source("./functions/functions_plot_nba_ranking.R")


#==== NBA color palette ====
nba_color_palette <- import("data/NBA_Color_Palette.csv")


#==== 2018 - 2019 Season game results ====
nba_2018_2019 <- scrape_nba_scoreboard(
  start_date = "2018-10-16",
  end_date = Sys.Date() %>% as.character()
) %>% 
  # remove future games
  na.omit() %>% 
  # keep only legitimate games
  filter(
    home_team %in% unique(nba_color_palette$team_short_name),
    away_team %in% unique(nba_color_palette$team_short_name)
  )


#==== plot ====
plot_2018_2019 <- plot_nba_ranking(
  start_date = as.Date("2018-10-21"),
  end_date = as.Date("2019-04-14"),
  scoreboard_full = nba_2018_2019,
  nba_color_palette = nba_color_palette,
  title = "NBA Power Ranking - Colley's Method"
)


#==== output =====
svg("output/NBA_Ranking_2018-2019.svg", width = 3 * 4, height = 5 * 4)
print(plot)
dev.off()

png("output/NBA_Ranking_2018-2019.png", width = 880, height = 1500, type = "cairo")
print(plot)
dev.off()

# play sound when finished
beep(sound = 2)

