#' duplicate every single game's result by swapping the home/away teams
#' effectively disregarding the home/away distinction
#' (for efficient implementation of scoreboard based algorithms)
#' 
#' @param scoreboard a data.frame of scoreboard
#' 
#' @return a data.frame of duplicated scoreboard
#' 
duplicate_scoreboard <- function(scoreboard){
  
  scoreboard <- scoreboard %>% 
    select(
      away_team, home_team, away_team_score, home_team_score, everything()
    )
  
  scoreboard %>% 
    # swap the away/home columns
    select(
      away_team = home_team, 
      home_team = away_team, 
      away_team_score = home_team_score, 
      home_team_score = away_team_score,
      everything()
    ) %>% 
    # bind the original (unswapped) scoreboard
    bind_rows(scoreboard)
}


#' order the team from the highest rank to lowest
#'
#' @param ratings the ratings for all teams
#' @param teams the set of teams to rank
#' 
#' @return a vector of team names
#' 
format_ratings <- function(ratings, teams = unique(ratings$team)){
  
  ratings %>% 
    filter(team %in% teams) %>% 
    arrange(overall_rank) %>%
    pull(team)
}
