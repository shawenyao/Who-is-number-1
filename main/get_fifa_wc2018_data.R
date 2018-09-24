library(tidyverse)
library(rvest)

#' extract the odd/even elements
#'
#' @param vector the input vector
#' @param type one of the two: "odd" or "even"
#' 
#' @return the odd/even elements of vector
#' 
select_elements <- function(vector, type){
  
  if(type == "odd"){
    # select 1st, 3rd, 5th, ... elements
    vector[c(TRUE, FALSE)]
  }else if(type == "even"){
    # select 1st, 3rd, 5th, ... elements
    vector[c(FALSE, TRUE)]
  }else{
    stop("illegal type")
  }
}


# read from www.fifa.com
html <- read_html("https://www.fifa.com/worldcup/matches/") 

# parse the returned html
fifa_wc2018_results <- 
  tibble(
    date = html %>% 
      html_nodes(css = ".fi-mu__info__datetime") %>% 
      html_text(trim = TRUE) %>% 
      substr(1, 11) %>% 
      as.Date("%d %b %Y"),
    
    # extract [odd, even] pairs to recreate match-ups
    home_team = html %>% 
      html_nodes(css = ".fi-t__nText") %>% 
      html_text() %>% 
      select_elements(type = "odd"),
    
    away_team = html %>% 
      # select 2nd, 4th, 5th, ... elements
      html_nodes(css = ".fi-t__nText") %>% 
      html_text() %>% 
      select_elements(type = "even"),
    
    # extract [odd, even] pairs to recreate scoreboards
    home_team_score = html %>% 
      html_nodes(css = ".fi-s__scoreText") %>% 
      html_text(trim = TRUE) %>% 
      str_split("[-]") %>% 
      unlist() %>% 
      select_elements(type = "odd") %>% 
      as.numeric(),
    
    away_team_score = html %>% 
      html_nodes(css = ".fi-s__scoreText") %>% 
      html_text(trim = TRUE) %>% 
      str_split("[-]") %>% 
      unlist() %>% 
      select_elements(type = "even") %>% 
      as.numeric()
  )

setwd("C:/Users/Wenyao/Desktop/R/Who-is-number-1")
save(fifa_wc2018_results, file = "./data/FIFA_WC2018.RData")

