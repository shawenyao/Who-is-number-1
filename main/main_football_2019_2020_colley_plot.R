suppressWarnings(library(zoo))
suppressWarnings(library(tidyverse))
suppressWarnings(library(rvest))
suppressWarnings(library(magrittr))
suppressWarnings(library(rio))
suppressWarnings(library(beepr))
suppressWarnings(library(png))
suppressWarnings(library(grid))

set.seed(350)


#==== general setup ====

# whether to scrape the scoreboard from web or load existing scoreboard
refresh_scoreboard <- TRUE

setwd("C:/Users/Wenyao/Desktop/R/Who-is-number-1")

source("functions/functions_scrape_bbc.R")
source("functions/functions_general.R")
source("functions/functions_colley's_method.R")
source("functions/functions_plot_football_ranking.R")

# the list of football clubs (for plotting logos)
fc_logos_file <- "data/european_football_clubs.csv"

# the match results file
scoreboard_file <- "data/football_2019_2020.csv"


#==== input ====
# the logo files
fc_logos <- import(fc_logos_file, encoding = "UTF-8")

# the match results
if(!isTRUE(refresh_scoreboard)){
  
  # read exisiting game results file
  scoreboard <- scoreboard_file %>% import(encoding = "UTF-8")
  
}else{
  
  scoreboard <- c(
    "premier-league",
    "spanish-la-liga",
    "italian-serie-a",
    "german-bundesliga",
    "french-ligue-one",
    "champions-league",
    "europa-league"
  ) %>% 
    # scrape bbc.com
    map(
      scrape_bbc,
      start = as.yearmon("2019-08-01"), 
      end = as.yearmon(Sys.Date())
    ) %>% 
    bind_rows()
  
  # stop if there is NA in the scoreboard scrape
  stopifnot(all(!is.na(scoreboard)))
  
  # save a copy
  export(scoreboard, scoreboard_file)
}


# the team names in the big 5 leagues
big_5_teams <- scoreboard %>% 
  filter(
    !league %in% c("champions-league", "europa-league")
  ) %>% 
  select(home_team, away_team) %>% 
  unlist() %>% 
  unique()

# only include teams in the big 5 leagues
scoreboard <- scoreboard %>% 
  filter(
    home_team %in% big_5_teams,
    away_team %in% big_5_teams
  )


#==== plot ====
plot_2019_2020 <- plot_football_ranking(
  ranking_start_date = as.Date("2019-09-08"),
  ranking_end_date = as.Date("2019-10-06"),
  frequency = 7,
  scoreboard = scoreboard,
  fc_logos = fc_logos,
  title = "European Football Club 2019-20 Season Power Rankings"
)


#==== output =====
svg(paste0("output/footbal_2019_2020/European_Football_Club_2019_20_Season_Power_Rankings.svg"), width = 3 * 4, height = 10 * 4)
print(plot_2019_2020)
dev.off()

png(paste0("output/footbal_2019_2020/European_Football_Club_2019_20_Season_Power_Rankings.png"), width = 880, height = 3000, type = "cairo")
print(plot_2019_2020)
dev.off()

# play sound when finished
beep(sound = 2)
