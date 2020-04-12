#' Colley's rating method
#'
#' @param scoreboard score info for each match
#'
#' @return a data.frame of the final ratings
#' 
colleys_method <- function(scoreboard){
  
  #==== duplicate every single game by swapping the home/away teams ====
  # effectively disregarding the home/away distinction
  scoreboard_doubled <- duplicate_scoreboard(scoreboard)
  
  
  #==== calculate points scored and net points scored for each team ====
  team_performance <- scoreboard_doubled %>%
    rename(team = home_team) %>% 
    group_by(team) %>% 
    summarise(
      net_wins = sum(home_team_score > away_team_score) - sum(home_team_score < away_team_score)
    )
  
  
  #==== count how many times each team has played against each other ====
  massey_matrix <- scoreboard_doubled %>%
    select(home_team, away_team) %>% 
    # create dummies indicating whether two teams have played
    model.matrix( ~ away_team + 0, .) %>%
    as_tibble() %>% 
    bind_cols(
      scoreboard_doubled %>% select(home_team)
    ) %>% 
    # aggregating the dummies grouped by the same home team
    # (due to the duplication in the first step, effectively treating away team as home team as well)
    group_by(home_team) %>% 
    summarise_all(sum) %>% 
    # convert to data.frame to allow the use of column_to_rownames()
    as.data.frame() %>% 
    # convert the column home_team to row name
    column_to_rownames(var = "home_team") %>% 
    # get rid of the string "away_team" in the column names
    set_colnames(colnames(.) %>% gsub("away_team", "", .)) %>% 
    as.matrix()
  
  # the opposite of total games played pair-wise
  massey_matrix <- -massey_matrix
  
  # set the diagonal elements to be the total number of games played by each team
  diag(massey_matrix) <- -colSums(massey_matrix)
  
  # C = M + 2I
  colley_matrix <- massey_matrix + 2 * diag(nrow(massey_matrix))
  
  # b = 1 + 1/2 * (win - loss)
  b <- 1 + 1/2 * team_performance$net_wins
  
  # make sure the order is consistent
  all(team_performance$team == colnames(colley_matrix)) %>% stopifnot()
  all(colnames(colley_matrix) == rownames(colley_matrix)) %>% stopifnot()
  
  
  #==== the Colley's method ====
  # solve for the overall Massey ratings
  # C r = b
  colley_ratings <- solve(
    a = colley_matrix,
    b = b,
    tol = .Machine$double.xmin
  )

  
  #==== final ratings====
  final_ratings <- tibble(
    team = names(colley_ratings),
    overall_rating = colley_ratings,
    overall_rank = rank(-colley_ratings)
  )
}
