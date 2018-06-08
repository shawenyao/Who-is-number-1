library(magrittr)
library(tidyverse)
library(rio)

setwd("C:/Users/Wenyao/Desktop/R/Who-is-number-1")
source("./functions_masseys_method.R")


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
ratings_1y <- masseys_method(scoreboard = fifa %>% filter(date >= "2017-06-07"))
ratings_2y <- masseys_method(scoreboard = fifa %>% filter(date >= "2016-06-07"))
ratings_4y <- masseys_method(scoreboard = fifa %>% filter(date >= "2014-06-07"))


#==== load World Cup 2018 groups ====
world_cup <- tibble(
  group = LETTERS[1:8] %>% rep(each = 4),
  team = c(
    "Russia", "Saudi Arabia", "Egypt", "Uruguay",
    "Portugal", "Spain", "Morocco", "Iran",
    "France", "Australia", "Peru", "Denmark",
    "Argentina", "Iceland", "Croatia", "Nigeria",
    "Brazil", "Switzerland", "Costa Rica", "Serbia",
    "Germany", "Mexico", "Sweden", "Korea Republic",
    "Belgium", "Panama", "Tunisia", "England",
    "Poland", "Senegal", "Colombia", "Japan"
  )
)

#==== the group statge matchup ====
matchups <- world_cup %>% 
  split(.$group) %>% 
  map(function(df){
    # all possible combinations of matchups within a group
    combn(df$team, 2) %>%
      t() %>% 
      as.tibble() %>% 
      set_colnames(c("away_team", "home_team"))
  }) %>% 
  bind_rows(.id = "group")

#==== predict final scores using the 3 ratings====
# the group stage
matchups_results <- matchups %>% 
  mutate(
    result = pmap(
      .,
      function(group, away_team, home_team){
        list(
          ratings_1y = ratings_1y,
          ratings_2y = ratings_2y,
          ratings_4y = ratings_4y
        ) %>% 
          map(
            predict_score,
            away_team = away_team,
            home_team = home_team
          ) %>% 
          bind_rows(.id = "method") %>% 
          select(-away_team, -home_team)
      }
    )
  ) %>% 
  unnest()

# aggregate match results across the 3 ratings
matchups_results_agg <- matchups_results %>% 
  group_by(group, away_team, home_team) %>% 
  summarise(
    away_team_score = mean(away_team_score),
    home_team_score = mean(home_team_score)
  )

# calcualte points for each game
points_by_match <- 
  # duplicate every single game by swapping the home/away teams
  matchups_results_agg %>% 
  select(
    group,
    away_team = home_team, 
    home_team = away_team, 
    away_team_score = home_team_score, 
    home_team_score = away_team_score
  ) %>% 
  bind_rows(matchups_results_agg) %>% 
  mutate(
    away_team_score = away_team_score %>% round(),
    home_team_score = home_team_score %>% round(),
    # translate win/draw/lose to points
    home_team_points = case_when(
      home_team_score > away_team_score ~ 3,
      home_team_score == away_team_score ~ 1,
      home_team_score < away_team_score ~ 0
    )
  )

# calculate the standings for each group
points_by_group <- points_by_match %>% 
  rename(team = home_team) %>% 
  group_by(group, team) %>% 
  summarise(points = sum(home_team_points)) %>% 
  arrange(group, -points)


# the knockout stage
group_stage_standings <- points_by_group %>% 
  slice(1:2) %>% 
  mutate(
    team_code = paste0(group, row_number())
  )

# knockout round 1 matchup
round1_matchup <- tibble(
  home_team_code = c("A1", "C1", "E1", "G1", "B1", "D1", "F1", "H1"),
  away_team_code = c("B2", "D2", "F2", "H2", "A2", "C2", "E2", "G2")
)

group_stage_standings %>% 
  rename(home_team = team) %>% 
  left_join(round1_matchup, by = c("team_code" = "home_team_code")) %>% 
  left_join(group_stage_standings %>% ungroup() %>% select(team, team_code), by = c("away_team_code" = "team_code")) %>% 
  rename(away_team = team)
