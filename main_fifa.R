library(magrittr)
library(tidyverse)
library(rio)

setwd("C:/Users/Wenyao/Desktop/R/Who-is-number-1")
source("./functions_general.R")
source("./functions_fifa.R")
source("./functions_massey's_method.R")
source("./functions_colley's_method.R")

#==== load the historical scoreboard data ====
fifa <- import("./Data/FIFA_1982-2018.csv") %>% 
  as.tibble() %>% 
  select(
    date,
    away_team,
    home_team,
    away_team_score = away_score,
    home_team_score = home_score
  ) %>% 
  mutate(
    date = as.Date(date)
  )


#==== apply Massey's method the rate team performance ====
massey_ratings_1y <- masseys_method(scoreboard = fifa %>% filter(date >= "2017-06-01"))
massey_ratings_2y <- masseys_method(scoreboard = fifa %>% filter(date >= "2016-06-01"))
massey_ratings_4y <- masseys_method(scoreboard = fifa %>% filter(date >= "2014-06-01"))

# collect ratings list
ratings_list <- list(
  ratings_1y = massey_ratings_1y,
  ratings_2y = massey_ratings_2y,
  ratings_4y = massey_ratings_4y
)


#==== apply Colley's method the rate team performance ====
colley_ratings_1y <- colleys_method(scoreboard = fifa %>% filter(date >= "2017-06-01"))
colley_ratings_2y <- colleys_method(scoreboard = fifa %>% filter(date >= "2016-06-01"))
colley_ratings_4y <- colleys_method(scoreboard = fifa %>% filter(date >= "2014-06-01"))


#==== simulate the match scores based on Massey's ratings ====
source("./fifa_group_stage.R")
source("./fifa_knockout_stage.R")


#==== create rankings comparison summary ====
rankings_summary <- tibble(
  `Massey's Method @ 1Y` = massey_ratings_1y %>% format_ratings(group_stage_teams$team),
  `Colley's Method @ 1Y` = colley_ratings_1y %>% format_ratings(group_stage_teams$team),
  
  `Massey's Method @ 2Y` = massey_ratings_2y %>% format_ratings(group_stage_teams$team),
  `Colley's Method @ 2Y` = colley_ratings_2y %>% format_ratings(group_stage_teams$team),
  
  `Massey's Method @ 4Y` = massey_ratings_4y %>% format_ratings(group_stage_teams$team),
  `Colley's Method @ 4Y` = colley_ratings_4y %>% format_ratings(group_stage_teams$team)
)
