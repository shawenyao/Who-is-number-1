#==== round of 16 ====
round_of_16_teams <- points_by_team %>% 
  slice(1:2) %>% 
  transmute(
    team = team,
    team_code = paste0(group, row_number())
  ) %>% 
  ungroup()

round_of_16_matchups_rules <- tibble(
  home_team_code = c("A1", "C1", "E1", "G1", "B1", "D1", "F1", "H1"),
  away_team_code = c("B2", "D2", "F2", "H2", "A2", "C2", "E2", "G2")
)

round_of_16_matchups <- find_matchups(
  teams = round_of_16_teams, 
  matchup_rules = round_of_16_matchups_rules
)

round_of_16_results <- combine_multiple_predictions(
  ratings_list = ratings_list,
  matchups = round_of_16_matchups
)


#==== quarter-finals ====
quarter_finals_teams <- round_of_16_results %>% 
  duplicate_scoreboard() %>% 
  filter(away_team_score > home_team_score) %>% 
  select(team = away_team, team_code = group)

quarter_finals_matchups_rules <- tibble(
  home_team_code = c("A1B2", "E1F2", "B1A2", "F1E2"),
  away_team_code = c("C1D2", "G1H2", "D1C2", "H1G2")
)

quarter_finals_matchups <- find_matchups(
  teams = quarter_finals_teams, 
  matchup_rules = quarter_finals_matchups_rules
)

quarter_finals_results <- combine_multiple_predictions(
  ratings_list = ratings_list,
  matchups = quarter_finals_matchups
)


#==== semi-finals ====
semi_finals_teams <- quarter_finals_results %>% 
  duplicate_scoreboard() %>% 
  filter(away_team_score > home_team_score) %>% 
  select(team = away_team, team_code = group)

semi_finals_matchups_rules <- tibble(
  home_team_code = c("B1A2D1C2", "E1F2G1H2"),
  away_team_code = c("F1E2H1G2", "A1B2C1D2")
)

semi_finals_matchups <- find_matchups(
  teams = semi_finals_teams, 
  matchup_rules = semi_finals_matchups_rules
)

semi_finals_results <- combine_multiple_predictions(
  ratings_list = ratings_list,
  matchups = semi_finals_matchups
)


#==== third place play-off ====
third_place_playoff_teams <- semi_finals_results %>% 
  duplicate_scoreboard() %>% 
  filter(away_team_score < home_team_score) %>% 
  select(team = away_team, team_code = group)

third_place_playoff_results <- combine_multiple_predictions(
  ratings_list = ratings_list,
  matchups = tibble(
    group = "3rd-place Playoff",
    home_team = third_place_playoff_teams$team[1], 
    away_team = third_place_playoff_teams$team[2]
  )
)

#==== finals ====
finals_teams <- semi_finals_results %>% 
  duplicate_scoreboard() %>% 
  filter(away_team_score > home_team_score) %>% 
  select(team = away_team, team_code = group)

finals_results <- combine_multiple_predictions(
  ratings_list = ratings_list,
  matchups = tibble(
    group = "Final",
    home_team = finals_teams$team[1], 
    away_team = finals_teams$team[2]
  )
)
