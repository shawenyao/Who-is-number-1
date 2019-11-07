suppressWarnings(library(jsonlite))
suppressWarnings(library(zoo))
suppressWarnings(library(tidyverse))
suppressWarnings(library(magrittr))
suppressWarnings(library(rio))
suppressWarnings(library(beepr))

set.seed(350)


#==== general setup ====
setwd("C:/Users/Wenyao/Desktop/R/Who-is-number-1")
source("functions/functions_scrape_nba.R")
source("functions/functions_general.R")
source("functions/functions_colley's_method.R")
source("functions/functions_plot_nba_ranking.R")

# whether to scrape the scoreboard from web or load existing scoreboard
refresh_scoreboard <- FALSE

# the match results
scoreboard_file <- "data/nba_2018-2019.csv"


#==== NBA color palette ====
nba_color_palette <- import("data/NBA_Color_Palette.csv")


#==== game results ====
if(isTRUE(refresh_scoreboard)){
  
  # scrape NBA.com
  scoreboard <- scrape_nba_scoreboard(
    start_date = as.Date("2018-10-16"),
    end_date = Sys.Date()
  ) %>% 
    # keep only legitimate games (i.e., remove all-stars)
    filter(
      home_team %in% unique(nba_color_palette$team_short_name),
      away_team %in% unique(nba_color_palette$team_short_name)
    )
  
  # stop if there is NA in the scoreboard scrape
  stopifnot(all(!is.na(scoreboard)))
  
  # save a copy
  export(scoreboard, scoreboard_file)
  
}else{
  
  # read exisiting game results file
  scoreboard <- scoreboard_file %>% 
    import(colClasses = c("date" = "character"))
}


#==== plot ====
plot <- plot_nba_ranking(
  ranking_start_date = as.Date("2018-10-21"),
  ranking_end_date = as.Date("2019-06-16"),
  scoreboard_full = scoreboard,
  frequency = 7,
  nba_color_palette = nba_color_palette,
  title = "NBA 2018-2019 Season Power Ranking - Colley's Method"
)


#==== output =====
svg("output/nba_2018-2019/NBA_Rankings_2018-2019.svg", width = 3 * 4, height = 5 * 4)
print(plot)
dev.off()

png("output/nba_2018-2019/NBA_Rankings_2018-2019.png", width = 880, height = 1500, type = "cairo")
print(plot)
dev.off()

# play sound when finished
beep(sound = 2)
