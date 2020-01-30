suppressWarnings(library(jsonlite))
suppressWarnings(library(zoo))
suppressWarnings(library(tidyverse))
suppressWarnings(library(magrittr))
suppressWarnings(library(rio))
suppressWarnings(library(parallel))
suppressWarnings(library(beepr))

set.seed(350)


#==== general setup ====
setwd("C:/Users/Wenyao/Desktop/R/Who-is-number-1")
source("functions/functions_scrape_nba.R")
source("functions/functions_general.R")
source("functions/functions_colley's_method.R")
source("functions/functions_plot_nba_ranking.R")

# whether to scrape the scoreboard from web or load existing scoreboard
refresh_scoreboard <- TRUE

# the match results
scoreboard_file <- "data/nba_2019-2020.csv"


#==== NBA color palette ====
nba_color_palette <- import("data/NBA_Color_Palette.csv")


#==== game results ====
# read exisiting game results file
if(file.exists(scoreboard_file)){
  scoreboard <- scoreboard_file %>% 
    import(colClasses = c("date" = "character"))
}else{
  scoreboard <- NULL
}

# scrape NBA.com for incremental scoreboard information
if(isTRUE(refresh_scoreboard)){
  
  # scrape the entire season's scoreboard
  scoreboard_from_web <- scrape_nba_scoreboard(
    start_date = as.Date("2019-10-22"),
    end_date = Sys.Date() - 1,
    no_threads = detectCores() - 1
  ) %>% 
    # keep only legitimate games (i.e., remove all-stars)
    filter(
      home_team %in% unique(nba_color_palette$team_short_name),
      away_team %in% unique(nba_color_palette$team_short_name)
    ) %>% 
    # remove the rescheduled match between LAC and LAL
    filter(
      !(date == "20200128" & away_team == "LAC" & home_team == "LAL" & is.na(away_team_score))
    )
  
  # the overlapping scoreboard between web and local file
  # for reconciling purposes
  scoreboard_old <- scoreboard_from_web %>% 
    na.omit() %>% 
    inner_join(scoreboard, by = c("date", "away_team", "home_team"))
  
  # check for discrepancies in history
  stopifnot(all(scoreboard_old$away_team_score.x == scoreboard_old$away_team_score.y))
  stopifnot(all(scoreboard_old$home_team_score.x == scoreboard_old$home_team_score.y))
  
  # the incremental scoreboard from the web compared to local file
  # can't have any NA
  scoreboard_new <- scoreboard_from_web %>% 
    anti_join(scoreboard, by = c("date", "away_team", "home_team"))
  
  # check for NA
  stopifnot(sum(is.na(scoreboard_new)) == 0)
  
  # bind exisiting and new scoreboard information
  scoreboard <- bind_rows(scoreboard, scoreboard_new) %>% 
    arrange(date)
  
  # save a copy
  export(scoreboard, scoreboard_file)
}


#==== plot ====
plot <- plot_nba_ranking(
  ranking_start_date = as.Date("2019-10-27"),
  ranking_end_date = Sys.Date() - 1,
  scoreboard_full = scoreboard,
  frequency = 7,
  nba_color_palette = nba_color_palette,
  title = "NBA 2019-2020 Season Power Ranking - Colley's Method"
)


#==== output =====
svg("output/nba_2019-2020/NBA_Rankings_2019-2020.svg", width = 3 * 4, height = 5 * 4)
print(plot)
dev.off()

png("output/nba_2019-2020/NBA_Rankings_2018-2020.png", width = 880, height = 1500, type = "cairo")
print(plot)
dev.off()

# play sound when finished
beep(sound = 2)
