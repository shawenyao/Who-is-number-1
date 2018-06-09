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
#' @param start_date
#' @param end_date
#' 
#' @return a data.frame
#' 
scrape_nba_scoreboard <- function(start_date, end_date){
  
  game_days <- seq(
    from = start_date %>% as.Date(),
    to = end_date %>% as.Date(),
    by = "day"
  ) %>% 
    format("%Y%m%d")
  
  scoreboard <- game_days %>% 
    paste0(
      "https://data.nba.net/prod/v2/",
      .,
      "/scoreboard.json"
    ) %>% 
    map(readLines) %>% 
    map(fromJSON) %>% 
    map(nba_json_to_df) %>% 
    bind_rows()
}
