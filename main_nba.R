library(magrittr)
library(tidyverse)
library(rio)

setwd("C:/Users/Wenyao/Desktop/R/Who-is-number-1")
source("./functions_general.R")
source("./functions_massey's_method.R")
source("./functions_colley's_method.R")


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
massey_regular_season_ratings <- masseys_method(scoreboard = nba_regular_season)
massey_playoffs_ratings <- masseys_method(scoreboard = nba_playoffs)
massey_overall_ratings <- masseys_method(scoreboard = bind_rows(nba_regular_season, nba_playoffs))


#==== apply Colley's method the rate team performance ====
colley_regular_season_ratings <- colleys_method(scoreboard = nba_regular_season)
colley_playoffs_ratings <- colleys_method(scoreboard = nba_playoffs)
colley_overall_ratings <- colleys_method(scoreboard = bind_rows(nba_regular_season, nba_playoffs))


#==== create rankings comparison summary ====
rankings_summary <- tibble(
  Rank = 1:30,
  
  `Massey's Method @ Regular Season` = massey_regular_season_ratings %>% format_ratings(),
  `Colley's Method @ Regular Season` = colley_regular_season_ratings %>% format_ratings(),
  
  `Massey's Method @ Playoffs` = massey_playoffs_ratings %>% format_ratings() %>% c(rep("", 14)),
  `Colley's Method @ Playoffs` = colley_playoffs_ratings %>% format_ratings() %>% c(rep("", 14)),
  
  `Massey's Method @ Overall` = massey_overall_ratings %>% format_ratings(),
  `Colley's Method @ Overall` = colley_overall_ratings %>% format_ratings()
)


#==== for fun: predict GSW vs CLE scores based on Massey's method ====
list(
  regular_season_ratings = massey_regular_season_ratings,
  playoffs_ratings = massey_overall_ratings,
  overall_ratings = massey_overall_ratings
) %>% 
  map(
    predict_score,
    away_team = "GSW",
    home_team = "CLE"
  ) %>% 
  bind_rows(.id = "method")
