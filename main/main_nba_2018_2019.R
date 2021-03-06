library(jsonlite)
library(zoo)
library(tidyverse)
library(magrittr)
library(rio)

setwd("C:/Users/Wenyao/Desktop/R/Who-is-number-1")
source("./functions/functions_scrape_nba.R")
source("./functions/functions_general.R")
source("./functions/functions_massey's_method.R")
source("./functions/functions_colley's_method.R")


#==== 2018 - 2019 Season ====
regular_season_2018_2019 <- scrape_nba_scoreboard(
  start_date = "2018-10-16",
  end_date = Sys.Date() %>% as.character()
) %>% 
  # remove future games
  na.omit()


#==== apply Massey's method the rate team performance ====
massey_regular_season_ratings <- masseys_method(scoreboard = regular_season_2018_2019)


#==== apply Colley's method the rate team performance ====
colley_regular_season_ratings <- colleys_method(scoreboard = regular_season_2018_2019)


#==== create rankings comparison summary ====
rankings_summary <- tibble(
  Rank = 1:30,
  
  `Massey's Method` = massey_regular_season_ratings %>% format_ratings(),
  `Colley's Method` = colley_regular_season_ratings %>% format_ratings()
)


#==== convert to jekyll-friendly format and copy to clipboard ====
rankings_summary %>%
  add_row(Rank = "---", `Massey's Method` = "---", `Colley's Method` = "---", .before = 1) %>% 
  mutate(Rank = paste0("|", Rank)) %>% 
  rename(`|Rank` = Rank) %>% 
  write.table("clipboard-128", row.names = FALSE, sep = "|", quote = FALSE, eol = "|\n")
