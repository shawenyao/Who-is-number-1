#' query match results for one league for multiple months
#'
#' @param start start date in yearmon format
#' @param end end date in yearmon format
#' @param league name of the league
#' @param no_threads enable parallel scraping if greater than 1
#' 
#' @return a data.frame of match results
#' 
scrape_bbc <- function(start, end, league, no_threads = 1){
  
  print(paste0("==== Now scraping ", league, " ===="))
  
  game_months <- seq(start, end, by = 1/12)

  if(no_threads == 1){
    # single-core scraping
    scoreboard <- map(game_months, scrape_one_month, league = league) 
  }else{
    # parallel scraping
    cl <- makeCluster(no_threads)
    clusterEvalQ(cl, lapply(c("dplyr", "stringr", "rvest", "zoo"), library, character.only = TRUE))
    scoreboard <- parLapply(cl, game_months, scrape_one_month, league = league)
    stopCluster(cl)
  }
  
  print(paste0("==== Finished scraping ", league, " ===="))
  
  scoreboard %>% 
    bind_rows() %>% 
    arrange(date)
}


#' query match results for one league for one month
#' 
#' @param game_month the month to query in yearmon format
#' @param league name of the league
#' 
#' @return a data.frame of match results of the month
#' 
scrape_one_month <- function(game_month, league){
  
  print(paste0(league, " @ ", game_month))
  
  year <- game_month %>% format("%Y")
  month <- game_month %>% format("%m")
  
  # read the match results page from bbc.com
  page <- paste0("https://www.bbc.com/sport/football/", league, "/scores-fixtures/", year, "-", month, "?filter=results") %>% 
    read_html()
  
  # character vectors of home/away team names
  # needs to be cleaned due to
  # 1) contains dates as well
  # 2) contains duplicate names for each team, once for short name the other for full name
  home_team <- page %>% 
    html_nodes(".sp-c-fixture__team-name--home .sp-c-fixture__team-name-trunc , .sp-c-match-list-heading") %>% 
    html_text()
  away_team <- page %>% 
    html_nodes(".sp-c-match-list-heading , .sp-c-fixture__team-name--away .sp-c-fixture__team-name-trunc") %>%
    html_text()
  
  # numeric vectors of home/away team scores
  home_team_score <- page %>% 
    html_nodes(".sp-c-fixture__number--home") %>% 
    html_text() %>% 
    as.numeric()
  away_team_score <- page %>% 
    html_nodes(".sp-c-fixture__number--away") %>% 
    html_text() %>% 
    as.numeric()
  
  # clean the date elements
  scoreboard <- tibble(
    home_team = home_team,
    away_team = away_team
  ) %>% 
    mutate(
      league = league,
      
      # detect date rows by checking if the element contains the current month
      date_indicator = str_detect(home_team, month.name[as.numeric(month)]),
      
      # the date is filled downwards to replace NAs
      date = if_else(date_indicator, home_team, NA_character_),
      date = na.locf(date),
      
      # extract date of the month from the character
      day = str_extract(date, "[\\d]+"),
      
      # construct the full date in Date format
      date = as.Date(paste0(year, "-", month, "-", day))
    ) %>% 
    # delete the rows that contain date
    filter(!date_indicator) %>% 
    select(-date_indicator, -day)
  
  scoreboard %>% 
    # the odd rows - short name
    slice(seq(from = 1, to = nrow(.), by = 2)) %>% 
    bind_cols(
      scoreboard %>% 
        # the even rows - full name
        slice(seq(from = 2, to = nrow(.), by = 2)) %>% 
        select(home_team_full = home_team, away_team_full = away_team)
    ) %>% 
    mutate(
      home_team_score = home_team_score,
      away_team_score = away_team_score
    ) %>% 
    arrange(date) %>% 
    select(
      league, date, 
      home_team, home_team_score, away_team, away_team_score,
      home_team_full, away_team_full
    )
}
