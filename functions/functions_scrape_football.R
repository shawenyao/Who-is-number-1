#'
#'
#' @param start
#' @param end
#' @param league name of the league
#' 
#' @return a data.frame of match results
#' 
scrape_bbc <- function(start, end, league){
  
  year_months <- seq(start, end, by = 1/12)
  
  year_months %>% 
    map(scrape_one_month, league = league) %>% 
    bind_rows()
  
}

#'
#'
scrape_one_month <- function(year_month, league){
  
  year <- year_month %>% format("%Y")
  month <- year_month %>% format("%m")
  
  page <- paste0("https://www.bbc.com/sport/football/", league, "/scores-fixtures/", year, "-", month, "?filter=results") %>% 
    read_html()
  
  home_team <- page %>% 
    html_nodes(".sp-c-fixture__team-name--home .sp-c-fixture__team-name-trunc , .sp-c-match-list-heading") %>% 
    html_text()
  
  away_team <- page %>% 
    html_nodes(".sp-c-match-list-heading , .sp-c-fixture__team-name--away .sp-c-fixture__team-name-trunc") %>% 
    html_text()
  
  home_team_score <- page %>% 
    html_nodes(".sp-c-fixture__number--home") %>% 
    html_text() %>% 
    as.numeric()
  
  away_team_score <- page %>% 
    html_nodes(".sp-c-fixture__number--away") %>% 
    html_text() %>% 
    as.numeric()
  
  # each match is repeated twice
  # once with short team name and once with full team
  scoreboard_2 <- tibble(
    home_team = home_team,
    away_team = away_team
  ) %>% 
    mutate(
      league = league,
      date_indicator = str_detect(home_team, month.name[as.numeric(month)]),
      date = if_else(date_indicator, home_team, NA_character_),
      date = na.locf(date),
      day = str_extract(date, "[\\d]+"),
      date = as.Date(paste0(year, "-", month, "-", day))
    ) %>% 
    filter(!date_indicator) %>% 
    select(-date_indicator, -day)
  
  scoreboard <- scoreboard_2 %>% 
    slice(seq(from = 1, to = nrow(.), by = 2)) %>% 
    bind_cols(
      scoreboard_2 %>% 
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





