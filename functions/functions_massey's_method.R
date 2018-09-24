#' Massey's rating method
#'
#' @param scoreboard score info for each match
#'
#' @return a data.frame of the final overall/offensive/defensive ratings
#' 
masseys_method <- function(scoreboard){
  
  #==== duplicate every single game by swapping the home/away teams ====
  # effectively disregarding the home/away distinction
  scoreboard_doubled <- duplicate_scoreboard(scoreboard)

  
  #==== calculate points scored and net points scored for each team ====
  team_performance <- scoreboard_doubled %>%
    rename(team = home_team) %>% 
    group_by(team) %>% 
    summarise(
      points_scored = sum(home_team_score),
      net_points_scored = sum(home_team_score - away_team_score)
    )
  
  
  #==== count how many times each team has played against each other ====
  massey_matrix <- scoreboard_doubled %>%
    select(home_team, away_team) %>% 
    # create dummies indicating whether two teams have played
    model.matrix( ~ away_team + 0, .) %>%
    as.tibble() %>% 
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
  
  
  #==== the Massey's method ====
  # make sure the order is consistent
  all(team_performance$team == colnames(massey_matrix)) %>% stopifnot()
  all(colnames(massey_matrix) == rownames(massey_matrix)) %>% stopifnot()
  
  # adjust the net points scored vector and Massey matrix to prevent being singular
  adj_net_points_scored <- team_performance$net_points_scored
  adj_net_points_scored[length(adj_net_points_scored)] <- 0
  
  adj_massey_matrix <- massey_matrix
  adj_massey_matrix[nrow(adj_massey_matrix), ] <- 1
  
  # solve for the overall Massey ratings
  massey_ratings <- solve(
    a = adj_massey_matrix,
    b = adj_net_points_scored,
    tol = .Machine$double.xmin
  )
  
  
  #==== the advanced Massey's method ====
  massey_matrix_T <- massey_matrix %>% diag() %>% diag()
  massey_matrix_P <- -massey_matrix
  diag(massey_matrix_P) <- 0
  
  # solve for the defensive ratings
  defensive_ratings <- solve(
    a = massey_matrix_T + massey_matrix_P,
    b = (massey_matrix_T %*% massey_ratings - team_performance$points_scored) %>% as.numeric(),
    tol = .Machine$double.xmin
  )
  
  # derive the offensive ratings
  offensive_ratings <- massey_ratings - defensive_ratings
  
  
  #==== final ratings====
  final_ratings <- tibble(
    team = names(massey_ratings),
    overall_rating = massey_ratings,
    overall_rank = rank(-massey_ratings),
    offensive_rating = offensive_ratings,
    offensive_rank = rank(-offensive_ratings),
    defensive_rating = defensive_ratings,
    defensive_rank = rank(-defensive_ratings)
  )
}


#' make predictions for the match score based offensive/defensive ratings
#' 
#' @param away_team the name of the away team
#' @param home_team the name of the home team
#' @param ratings the Massey's overall/offensive/defensive ratings
#' 
#' @return a data.frame
#' 
predict_score <- function(away_team, home_team, ratings){
  
  # find the ratings for the away/home team
  away_team_ratings <- ratings %>% filter(team == away_team)
  home_team_ratings <- ratings %>% filter(team == home_team)
  
  # predict scores
  tibble(
    away_team = away_team,
    home_team = home_team,
    away_team_score = away_team_ratings$offensive_rating - home_team_ratings$defensive_rating,
    home_team_score = home_team_ratings$offensive_rating - away_team_ratings$defensive_rating
  ) %>% 
    mutate(
      away_team_score = pmax(away_team_score, 0),
      home_team_score = pmax(home_team_score, 0)
    )
}


#' equally weight predictions from multiple sets of ratings
#'
#' @param ratings_list a list of ratings based on different methods
#' @param matchups a data.frame of matchups
#' 
#' @return a data.frame of match results predictions
#' 
combine_multiple_predictions <- function(ratings_list, matchups){
  
  matchups_results <- matchups %>% 
    mutate(
      # loop over the each matchup (i.e., each pair of away/home teams)
      result = pmap(
        .,
        function(group, away_team, home_team){
          # loop over each set of ratings
          ratings_list %>% 
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
  
  # aggregate match results across all ratings
  matchups_results_agg <- matchups_results %>% 
    group_by(group, away_team, home_team) %>% 
    summarise(
      away_team_score = mean(away_team_score),
      home_team_score = mean(home_team_score)
    ) %>% 
    ungroup()
}
