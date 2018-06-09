library(magrittr)
library(tidyverse)
library(rio)

setwd("C:/Users/Wenyao/Desktop/R/Who-is-number-1")
source("./functions_masseys_method.R")


#==== load the historical NBA scoreboard data ====
nba_regular_season <- import("./Data/NBA_2017-2018_Regular_Season.RData") %>% as.tibble()
nba_playoffs <- import("./Data/NBA_2017-2018_Playoffs.RData") %>% as.tibble()

nba_regular_season <- nba_regular_season %>%  
  # disregard all-star games
  filter(
    !home_team %in% c("STP", "USA", "WST")
  )

nba_playoffs <- nba_playoffs %>% 
  filter(!is.na(home_team_score))


#==== apply Massey's method the rate team performance ====
regular_season_ratings <- masseys_method(scoreboard = nba_regular_season)
playoffs_ratings <- masseys_method(scoreboard = nba_playoffs)
overall_ratings <- masseys_method(scoreboard = bind_rows(nba_regular_season, nba_playoffs))


#==== predict final scores ====
list(
  regular_season_ratings = regular_season_ratings,
  playoffs_ratings = playoffs_ratings,
  overall_ratings = overall_ratings
) %>% 
  map(
    predict_score,
    away_team = "GSW",
    home_team = "CLE"
  ) %>% 
  bind_rows(.id = "method")
