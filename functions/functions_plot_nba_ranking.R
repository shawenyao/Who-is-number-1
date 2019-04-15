#' plot the evolution of nba power ranking
#' 
#' @param ranking_start_date the first date of the ranking
#' @param ranking_end_date the last date of the ranking
#' @param frequency the time interval between two consecutive rankings
#' @param scoreboard_full the complete scoreboard data.frame
#' @param nba_color_palette the color palette data.frame
#' @param title plot of the title
#' 
#' @return a ggplot object
#' 
plot_nba_ranking <- function(
  ranking_start_date,
  ranking_end_date,
  frequency = 7,
  scoreboard_full,
  nba_color_palette,
  title
){
  
  #==== feed the match results incrementaly to the ranking algorithm ====
  # (the first ranking can be produced as early as the 2nd day
  # when all teams have at least had one game)
  as_of_dates <- seq(from = ranking_start_date, to = ranking_end_date, by = frequency)
  
  # for each training window, find the Colley ranking
  rankings <- as_of_dates %>% 
    map(function(as_of_date){
      tibble(
        rank = 1:30,
        team = colleys_method(
          scoreboard = scoreboard_full %>% 
            filter(as.Date(date, "%Y%m%d") <= as_of_date)
        ) %>% 
          format_ratings()
      )
    }) %>% 
    set_names(as_of_dates) %>% 
    bind_rows(.id = "as_of_date") %>% 
    mutate(
      as_of_date = as.Date(as_of_date),
      day = as.numeric(as_of_date - as.Date(min(scoreboard_full$date), "%Y%m%d") + 1)
    )
  
  # auto-adjust the width of team label
  label_width <- 0.07 * as.numeric(max(as_of_dates) - min(as_of_dates))
  
  
  #==== plot ====
  ggplot(data = rankings, aes(x = day, y = rank, group = team)) +
    geom_line(aes(alpha = 1, color = team), size = 2) +
    geom_point(aes(alpha = 1, color = team), size = 6) +
    geom_point(color = "white", size = 2) +
    scale_x_continuous(
      breaks = seq(from = min(rankings$day), to = max(rankings$day), by = frequency), 
      minor_breaks = seq(from = min(rankings$day), to = max(rankings$day), by = frequency), 
      expand = c(.05, .05),
      labels = as_of_dates %>% format("%b %d")
    ) +
    scale_y_reverse(breaks = 1:30, sec.axis = dup_axis()) +
    # the label background box on the left side
    geom_tile(
      data = rankings %>% filter(day == min(rankings$day)), 
      aes(x = min(rankings$day) - label_width, y = rank, fill = team, color = team),
      height = 0.6, 
      width = label_width,
      size = 1.3
    ) + 
    # the label background box on the right side
    geom_tile(
      data = rankings %>% filter(day == max(day)), 
      aes(x = max(rankings$day) + label_width, y = rank, fill = team, color = team),
      height = 0.6,
      width = label_width,
      size = 1.3
    ) + 
    # the label on the left side
    geom_text(
      data = rankings %>% filter(day == min(day)),
      aes(label = team, x = min(rankings$day) - label_width) , 
      fontface = "bold", 
      color = "white", 
      size = 5
    ) +
    # the label on the right side
    geom_text(
      data = rankings %>% filter(day == max(day)),
      aes(label = team, x = max(rankings$day) + label_width) ,
      fontface = "bold",
      color = "white",
      size = 5
    ) +
    scale_fill_manual(
      values = nba_color_palette$major_color %>% set_names(nba_color_palette$team_short_name)
    ) +
    scale_color_manual(
      values = nba_color_palette$minor_color %>% set_names(nba_color_palette$team_short_name)
    ) +
    labs(
      title = title,
      subtitle = paste0("Last updated on ", as_of_dates %>% tail(1) %>% format("%b %d, %Y"))
    ) +
    theme_bw(base_size = 20) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.04),
      plot.subtitle = element_text(hjust = 0.03),
      plot.margin = margin(0, 0, 0, 0, "cm"),
      axis.text.x = element_text(angle = 50),
      axis.text.y.left = element_text(margin = margin(0, -1.2, 0, 0, "cm")),
      axis.text.y.right = element_text(margin = margin(0, 0, 0, -1.2, "cm")),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.border = element_blank()
    )
}
