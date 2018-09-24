library(magrittr)
library(tidyverse)
library(rio)

setwd("C:/Users/Wenyao/Desktop/R/Who-is-number-1")
source("./functions/functions_general.R")
source("./functions/functions_massey's_method.R")
source("./functions/functions_colley's_method.R")

#==== load the historical scoreboard data ====
fifa_wc2018_results <- import("./data/FIFA_WC2018.RData")

massey_ratings <- masseys_method(scoreboard = fifa_wc2018_results)
colley_ratings <- colleys_method(scoreboard = fifa_wc2018_results)

tibble(
  Rank = 1:32,
  `Massey's Method` = massey_ratings %>% format_ratings(),
  `Colley's Method` = colley_ratings %>% format_ratings()
)
