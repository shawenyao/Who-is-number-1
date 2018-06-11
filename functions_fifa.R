#' enumerate the games matchup between possible teams
#' based on the matchup rules
#' 
#' @param teams
#' @param matchup_rules
#' 
#' @return a data.frame of all possible matchups
#' 
find_matchups <- function(teams, matchup_rules){
  
  teams %>% 
    rename(
      home_team = team,
      home_team_code = team_code
    ) %>% 
    left_join(matchup_rules, by = "home_team_code") %>% 
    left_join(
      teams %>% select(away_team = team, away_team_code = team_code),
      by = "away_team_code"
    ) %>% 
    drop_na() %>% 
    mutate(group = paste0(home_team_code, away_team_code)) %>% 
    select(-home_team_code, -away_team_code)
}
