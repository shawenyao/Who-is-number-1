library(magrittr)
library(tidyverse)
library(rio)

setwd("C:/Users/Wenyao/Desktop/R/Who-is-number-1/main/2022")
source("../../functions/functions_general.R")
source("../../functions/functions_fifa.R")
source("../../functions/functions_massey's_method.R")
source("../../functions/functions_colley's_method.R")


#==== load the historical scoreboard data ====
fifa <- import("https://raw.githubusercontent.com/martj42/international_results/master/results.csv") %>% 
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


#==== apply Colley's method the rate team performance ====
colley_ratings_1y <- colleys_method(scoreboard = fifa %>% filter(date >= "2021-06-01", date <= "2022-07-26"))
colley_ratings_2y <- colleys_method(scoreboard = fifa %>% filter(date >= "2020-06-01", date <= "2022-07-26"))
colley_ratings_4y <- colleys_method(scoreboard = fifa %>% filter(date >= "2018-06-01", date <= "2022-07-26"))


#==== group stage ====
group_stage_teams <- tibble(
  group = LETTERS[1:8] %>% rep(each = 4),
  team = c(
    "Qatar", "Ecuador", "Senegal", "Netherlands",
    "England", "Iran", "United States", "Wales",
    "Argentina", "Saudi Arabia", "Mexico", "Poland",
    "France", "Australia", "Denmark", "Tunisia",
    "Spain", "Costa Rica", "Germany", "Japan",
    "Belgium", "Canada", "Morocco", "Croatia",
    "Brazil", "Serbia", "Switzerland", "Cameroon",
    "Portugal", "Ghana", "Uruguay", "South Korea"
  )
)


#==== create rankings comparison summary ====
# 1-year rank
overall_rank <- colley_ratings_1y %>% 
  filter(team %in% group_stage_teams$team) %>% 
  arrange(-overall_rating) %>% 
  select(team, rating_1y = overall_rating) %>% 
  left_join(
    # 2-year rank
    colley_ratings_2y %>% 
      filter(team %in% group_stage_teams$team) %>% 
      arrange(-overall_rating) %>% 
      select(team, rating_2y = overall_rating),
    by = "team"
  ) %>% 
  left_join(
    # 4-year rank
    colley_ratings_4y %>% 
      filter(team %in% group_stage_teams$team) %>% 
      arrange(-overall_rating) %>% 
      select(team, rating_4y = overall_rating),
    by = "team"
  ) %>% 
  mutate(
    rating_overall = rating_1y * 0.5 + rating_2y * 0.3 + rating_4y * 0.2
  ) %>% 
  arrange(-rating_1y) %>% 
  mutate(ranking_1y = row_number()) %>% 
  arrange(-rating_2y) %>% 
  mutate(ranking_2y = row_number()) %>% 
  arrange(-rating_4y) %>% 
  mutate(ranking_4y = row_number()) %>% 
  arrange(-rating_overall) %>% 
  mutate(ranking_overall = row_number())

View(overall_rank)
