suppressWarnings(library(zoo))
suppressWarnings(library(tidyverse))
suppressWarnings(library(rvest))
suppressWarnings(library(magrittr))
suppressWarnings(library(rio))
suppressWarnings(library(parallel))
suppressWarnings(library(beepr))
suppressWarnings(library(png))
suppressWarnings(library(grid))

set.seed(350)


#==== general setup ====
# whether to scrape the scoreboard from web or load existing scoreboard
refresh_scoreboard <- FALSE

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
# the match results
if(isTRUE(refresh_scoreboard)){
  
  scoreboard <- list(
    league = c(
      "premier-league",
      "spanish-la-liga",
      "italian-serie-a",
      "german-bundesliga",
      "french-ligue-one",
      "champions-league",
      "europa-league"
    ),
    end = c(
      as.yearmon("2020-07-31"),
      as.yearmon("2020-07-31"),
      as.yearmon("2020-08-31"),
      as.yearmon("2020-06-30"),
      as.yearmon("2020-03-31"),
      as.yearmon("2020-08-31"),
      as.yearmon("2020-08-31")
    )
  ) %>% 
    # scrape bbc.com
    pmap(
      scrape_bbc,
      start = as.yearmon("2019-08-01"), 
      no_threads = detectCores() - 1
    ) %>% 
    bind_rows()
  
  print(paste0(sum(is.na(scoreboard)) / 2, " matches are missing scores!"))
  
  scoreboard <- scoreboard %>% na.omit()
  
  # save a copy
  export(scoreboard, scoreboard_file)
  
}else{
  
  # read exisiting game results file
  scoreboard <- scoreboard_file %>% import(encoding = "UTF-8")
}


# the team names in the big 5 leagues
big_5_teams <- scoreboard %>% 
  filter(
    !league %in% c("champions-league", "europa-league")
  ) %>% 
  select(home_team, away_team) %>% 
  unlist() %>% 
  unique()

# only include matches of which both teams belong to the big 5 leagues
scoreboard <- scoreboard %>% 
  filter(
    home_team %in% big_5_teams,
    away_team %in% big_5_teams
  )

# the logo files
fc_logos <- import(fc_logos_file, encoding = "UTF-8")


#==== plot ====
plot <- plot_football_ranking(
  ranking_start_date = as.Date("2019-09-08"),
  ranking_end_date = Sys.Date(),
  frequency = 28,
  scoreboard = scoreboard,
  fc_logos = fc_logos,
  title = "European Football Clubs 2019-2020 Season Power Rankings"
)


#==== output =====
svg(paste0("output/footbal_2019-2020/European_Football_Club_Rankings_2019-2020.svg"), width = 3 * 4, height = 10 * 4)
print(plot)
dev.off()

png(paste0("output/footbal_2019-2020/European_Football_Club_Rankings_2019-2020.png"), width = 880, height = 3000, type = "cairo")
print(plot)
dev.off()

# play sound when finished
beep(sound = 2)
