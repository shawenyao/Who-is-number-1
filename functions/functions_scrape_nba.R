#' extract useful info from a json object 
#' and put it into a data.frame
#' 
#' @param json a json object containing useful NBA scoreboard info
#' 
#' @return a data.frame
#' 
nba_json_to_df <- function(json){
  
  if(length(json$games)==0){
    out <- NULL
  }else{
    out <- tibble(
      date = json$games$startDateEastern,
      away_team = json$games$vTeam$triCode,
      home_team = json$games$hTeam$triCode,
      away_team_score = json$games$vTeam$score %>% as.numeric(),
      home_team_score = json$games$hTeam$score %>% as.numeric()
    )
  }
  
  out
}


#' scrape NBA.com to extract scoreboard info
#'
#' @param start_date the first date to extract game score
#' @param end_date the last date to extract game score
#' @param no_threads enable parallel scraping if greater than 1
#' 
#' @return a data.frame of match results
#' 
scrape_nba_scoreboard <- function(start_date, end_date, no_threads = 1){
  
  game_days <- seq(
    from = start_date,
    to = end_date,
    by = "day"
  ) %>% 
    format("%Y%m%d")
  
  urls <- paste0(
      "https://data.nba.net/prod/v2/",
      game_days,
      "/scoreboard.json"
    )
  
  if(no_threads == 1){
    # single-core scraping
    scoreboard <- map(urls, readLines, warn = FALSE)
  }else{
    # parallel scraping
    cl <- makeCluster(no_threads)
    scoreboard <- parLapply(cl, urls, readLines, warn = FALSE)
    stopCluster(cl)
  }
  
  scoreboard %>% 
    map(fromJSON) %>% 
    map(nba_json_to_df) %>% 
    bind_rows() %>% 
    arrange(date)
}
