library(magrittr)
library(tidyverse)
library(rio)

setwd("C:/Users/Wenyao/Desktop/R/Who-is-number-1")
source("./functions_masseys_method.R")
source("./functions_fifa.R")

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
ratings_1y <- masseys_method(scoreboard = fifa %>% filter(date >= "2017-06-01"))
ratings_2y <- masseys_method(scoreboard = fifa %>% filter(date >= "2016-06-01"))
ratings_4y <- masseys_method(scoreboard = fifa %>% filter(date >= "2014-06-01"))

ratings_list <- list(
  ratings_1y = ratings_1y,
  ratings_2y = ratings_2y,
  ratings_4y = ratings_4y
)


#==== simulate the games ====
source("./fifa_group_stage.R")
source("./fifa_knockout_stage.R")