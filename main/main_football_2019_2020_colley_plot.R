suppressWarnings(library(zoo))
suppressWarnings(library(tidyverse))
suppressWarnings(library(rvest))
suppressWarnings(library(magrittr))
suppressWarnings(library(rio))
suppressWarnings(library(beepr))

set.seed(350)


#==== general setup ====
setwd("C:/Users/Wenyao/Desktop/R/Who-is-number-1")
source("./functions/functions_scrape_skysports.R")
source("./functions/functions_general.R")
source("./functions/functions_colley's_method.R")

# the match results
scoreboard_file <- "data/football_2019_2020.csv"


#==== game results ====
if(file.exists(scoreboard_file)){
  
  # read exisiting game results file
  premier_2019_2020 <- scoreboard_file %>% 
    import(colClasses = c("date" = "character"))
  
}else{
  
  scoreboard <- c(
    "premier-league",
    "spanish-la-liga",
    "italian-serie-a",
    "german-bundesliga",
    "french-ligue-one",
    "champions-league",
    "europa-league"
  ) %>% 
    # scrape bbc.com
    map(
      scrape_bbc,
      start = as.yearmon("2019-08-01"), 
      end = as.yearmon("2019-09-01")
    ) %>% 
    bind_rows()
  
  # stop if there is NA in the scoreboard scrape
  stopifnot(all(!is.na(scoreboard)))
  
  # save a copy
  export(scoreboard, scoreboard_file)
}


#==== plot ====
plot_2019_2020 <- plot_nba_ranking(
  ranking_start_date = as.Date("2018-10-21"),
  ranking_end_date = as.Date("2019-06-16"),
  scoreboard_full = nba_2018_2019,
  frequency = 7,
  title = "NBA 2018-2019 Season Power Ranking - Colley's Method"
)


#==== output =====
svg("output/Premier_League_Ranking_2019-2020.svg", width = 3 * 4, height = 5 * 4)
print(plot_2019_2020)
dev.off()

png("output/Premier_League_Ranking_2019-2020.png", width = 880, height = 1500, type = "cairo")
print(plot_2019_2020)
dev.off()

# play sound when finished
beep(sound = 2)
