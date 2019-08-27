library(tidyverse)
library(rvest)
library(zoo)
library(magrittr)


scrape_premier_league <- function(){
  
  teams <- c(
    "Arsenal",
    "Aston Villa",
    "Bournemouth",
    "Brighton and Hove Albion",
    "Burnley",
    "Chelsea",
    "Crystal Palace",
    "Everton",
    "Leicester City",
    "Liverpool",
    "Manchester City",
    "Manchester United",
    "Newcastle United",
    "Norwich City",
    "Sheffield United",
    "Southampton",
    "Tottenham Hotspur",
    "Watford",
    "West Ham United",
    "Wolverhampton Wanderers"
  )
  
  page <- "https://www.skysports.com/premier-league-results/2019-20" %>% 
    read_html()
  
  div_match <- page %>% html_nodes(".site-layout-secondary__col1")
  
  keywords <- test %>% 
    html_text() %>% 
    strsplit("\n") %>% 
    extract2(1) %>% 
    str_squish()
  
  keywords_filtered <- keywords[
    keywords %in% teams |
      !is.na(as.numeric(keywords)) |
      grepl(" 2019", keywords, ignore.case = TRUE) |
      grepl(" 2020", keywords, ignore.case = TRUE) |
      grepl("day", keywords, ignore.case = TRUE)
    ][-c(1:21)]
  
  
  match_results <- NULL
  i <- 1
  while(i <= length(keywords_filtered)){
    
    if(grepl("2019", keywords_filtered[i], ignore.case = TRUE) |
       grepl("2020", keywords_filtered[i], ignore.case = TRUE)){
      
      year <- keywords_filtered[i]
      i <- i + 1
      
    }
    else if(grepl("day", keywords_filtered[i], ignore.case = TRUE)){
      
      day <- keywords_filtered[i]
      i <- i + 1
      
    }else{
      
      match_results <- bind_rows(
        match_results,
        tibble(
          year = year,
          day = day,
          home_team = keywords_filtered[i],
          away_team = keywords_filtered[i+3],
          home_team_score = keywords_filtered[i+1] %>% as.numeric(),
          away_team_score = keywords_filtered[i+2] %>% as.numeric()
        )
      )
      i <- i + 4
      
    }
  }
  
  match_results %>% 
    separate(year, c("month", "year")) %>% 
    mutate(
      date = paste0(month, " ", str_extract(day, "\\d+"), ", ", year) %>% as.Date("%B %d, %Y")
    ) %>% 
    select(date, home_team, away_team, home_team_score, away_team_score) %>% 
    View()
}
