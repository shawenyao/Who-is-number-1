library(jsonlite)
library(zoo)
library(tidyverse)

setwd("C:/Users/Wenyao/Desktop/R/Who-is-number-1")
source("./functions_scrape_nba.R")

regular_season <- scrape_nba_scoreboard(
  start_date = "2017-10-17",
  end_date = "2018-04-11"
)
playoffs <- scrape_nba_scoreboard(
  start_date = "2018-04-14",
  end_date = "2018-06-08"
)

save(regular_season, file = "./Data/NBA_2017-2018_Regular_Season.RData")
save(playoffs, file = "./Data/NBA_2017-2018_Playoffs.RData")
