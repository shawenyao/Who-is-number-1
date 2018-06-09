#==== group stage ====
group_stage_teams <- tibble(
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

group_stage_matchups <- group_stage_teams %>% 
  split(.$group) %>% 
  map(function(teams_of_a_group){
    # all possible combinations of matchups within a group
    combn(teams_of_a_group$team, 2) %>%
      t() %>% 
      as.tibble() %>% 
      set_colnames(c("away_team", "home_team"))
  }) %>% 
  bind_rows(.id = "group")

group_stage_results <- combine_multiple_predictions(
  ratings_list = ratings_list,
  matchups = group_stage_matchups
)

points_by_match <- group_stage_results %>% 
  duplicate_scoreboard() %>% 
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

points_by_team <- points_by_match %>% 
  rename(team = home_team) %>% 
  group_by(group, team) %>% 
  summarise(points = sum(home_team_points)) %>% 
  arrange(group, -points)
