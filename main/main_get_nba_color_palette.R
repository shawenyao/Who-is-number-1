library(tidyverse)
library(rvest)
library(rio)
library(magrittr)

setwd("C:/Users/Wenyao/Desktop/R/Who-is-number-1")


#==== read the web page containing the color palette of every NBA team ====
nba_color_html <- read_html("https://teamcolorcodes.com/nba-team-color-codes/") %>% 
  html_nodes(".team-button") %>% 
  head(-1)


#==== parse the html css ====
# parse css into named vectors
# e.g., "background-color: #E03A3E; color: white; border-bottom: 4px solid #C1D32F;"
# will become 
# c("background-color" = "#E03A3E", "color" = "white", border-bottom = "4px solid #C1D32F")
css_to_vector <- function(css){
  
  key_value_pairs <- strsplit(css, ";")[[1]] %>% str_trim()
  
  values <- key_value_pairs %>%
    strsplit(":") %>%
    map(last) %>% 
    map(str_trim) %>% 
    unlist()
  
  keys <- key_value_pairs %>% 
    strsplit(":") %>% 
    map(first) %>% 
    map(str_trim) %>% 
    unlist()
  
  values %>% set_names(keys)
}

# collect major and minor color from css
nba_team_color_palette <- tibble(
  team = nba_color_html %>% html_text(),
  major_color = nba_color_html %>% 
    map(html_attr, name = "style") %>% 
    map(css_to_vector) %>% 
    map(extract2, "background-color") %>% 
    unlist(),
  minor_color = nba_color_html %>% 
    map(html_attr, name = "style") %>% 
    map(css_to_vector) %>% 
    map(extract2, "border-bottom") %>% 
    unlist() %>% 
    str_sub(start = regexpr("[#]", .))
) %>% 
  # fix for the white color code
  mutate(
    major_color = if_else(major_color == "#000", "#000000", major_color) %>% toupper(),
    minor_color = if_else(minor_color == "#000", "#000000", minor_color) %>% toupper()
  )


#==== conver full name to short name ====
team_name_lookup <- tribble(
  ~team_full_name, ~team_short_name,
  "Atlanta Hawks", "ATL",
  "Boston Celtics", "BOS",
  "Brooklyn Nets", "BKN",
  "Charlotte Hornets", "CHA",
  "Chicago Bulls", "CHI",
  "Cleveland Cavaliers", "CLE",
  "Dallas Mavericks", "DAL",
  "Denver Nuggets", "DEN",
  "Detroit Pistons", "DET",
  "Golden State Warriors", "GSW",
  "Houston Rockets", "HOU",
  "Indiana Pacers", "IND",
  "Los Angeles Clippers", "LAC",
  "Los Angeles Lakers", "LAL",
  "Memphis Grizzlies", "MEM",
  "Miami Heat", "MIA",
  "Milwaukee Bucks", "MIL",
  "Minnesota Timberwolves", "MIN",
  "New Orleans Pelicans", "NOP",
  "New York Knicks", "NYK",
  "Oklahoma City Thunder", "OKC",
  "Orlando Magic", "ORL",
  "Philadelphia 76ers", "PHI",
  "Phoenix Suns", "PHX",
  "Portland Trail Blazers", "POR",
  "Sacramento Kings", "SAC",
  "San Antonio Spurs", "SAS",
  "Toronto Raptors", "TOR",
  "Utah Jazz", "UTA",
  "Washington Wizards", "WAS"
)

output <- team_name_lookup %>% 
  left_join(nba_team_color_palette, by = c("team_full_name" = "team"))


#==== output ====
export(output, file = "data/NBA_Color_Palette.csv")
