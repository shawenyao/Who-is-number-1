suppressWarnings(library(jsonlite))
suppressWarnings(library(zoo))
suppressWarnings(library(tidyverse))
suppressWarnings(library(magrittr))
suppressWarnings(library(rio))
suppressWarnings(library(beepr))

set.seed(350)


#==== general setup ====
setwd("C:/Users/Wenyao/Desktop/R/Who-is-number-1")
source("./functions/functions_scrape_nba.R")
source("./functions/functions_general.R")
source("./functions/functions_colley's_method.R")
source("./functions/functions_plot_nba_ranking.R")

# the match results
scoreboard_file <- "data/nba_2018_2019.csv"


#==== NBA color palette ====
nba_color_palette <- import("data/NBA_Color_Palette.csv")


#==== game results ====
if(file.exists(scoreboard_file)){
  # read exisiting game results file
  nba_2018_2019 <- scoreboard_file %>% import(colClasses = c("date" = "character"))
}else{
  # scrape NBA.com
  nba_2018_2019 <- scrape_nba_scoreboard(
    start_date = as.Date("2018-10-16"),
    end_date = Sys.Date()
  ) %>% 
    # keep only legitimate games (i.e., remove all-stars)
    filter(
      home_team %in% unique(nba_color_palette$team_short_name),
      away_team %in% unique(nba_color_palette$team_short_name)
    )
  
  # stop if there is NA in the scoreboard scrape
  stopifnot(all(!is.na(nba_2018_2019)))
}


#==== plot ====
plot_2018_2019 <- plot_nba_ranking(
  ranking_start_date = as.Date("2018-10-21"),
  ranking_end_date = as.Date(Sys.Date()),
  scoreboard_full = nba_2018_2019,
  frequency = 7,
  nba_color_palette = nba_color_palette,
  title = "NBA 2018-2019 Season Power Ranking - Colley's Method"
)


#==== output =====
svg("output/NBA_Ranking_2018-2019.svg", width = 3 * 4, height = 5 * 4)
print(plot_2018_2019)
dev.off()

png("output/NBA_Ranking_2018-2019.png", width = 880, height = 1500, type = "cairo")
print(plot_2018_2019)
dev.off()

# play sound when finished
beep(sound = 2)
