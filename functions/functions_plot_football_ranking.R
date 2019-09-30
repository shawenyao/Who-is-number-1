#' plot the evolution of football power ranking
#' 
#' @param ranking_start_date the first date of the ranking
#' @param ranking_end_date the last date of the ranking
#' @param frequency the time interval between two consecutive rankings
#' @param scoreboard_full the complete scoreboard data.frame
#' @param fc_logos the football club logo file list
#' @param title plot of the title
#' 
#' @return a ggplot object
#' 
plot_football_ranking <- function(
  ranking_start_date,
  ranking_end_date,
  frequency = 7,
  scoreboard_full,
  fc_logos,
  title
){
  
  #==== team logos ====
  fc_logo_img_list <- paste0("icons/small/", fc_logos$id, ".png") %>% 
    map(readPNG)
  
  main_color <- fc_logo_img_list %>% 
    map(function(color_matrix){
      
      # exclude white and black color
      color_matrix[color_matrix > 0.99 | color_matrix < 0.01] <- NA
      
      # find the average color in the logo
      rgb(
        find_mode(color_matrix[,,1]),
        find_mode(color_matrix[,,2]),
        find_mode(color_matrix[,,3])
      )
    }) %>% 
    unlist() %>% 
    set_names(fc_logos$team)
  
  
  #==== feed the match results incrementaly to the ranking algorithm ====
  # (the first ranking can be produced as early as the 2nd day
  # when all teams have at least had one game)
  as_of_dates <- seq(from = ranking_start_date, to = ranking_end_date, by = frequency)
  
  # for each training window, find the Colley ranking
  rankings <- as_of_dates %>% 
    map(function(as_of_date){
      tibble(
        team = colleys_method(
          scoreboard = scoreboard_full %>% 
            filter(as.Date(date) <= as_of_date)
        ) %>% 
          format_ratings()
      ) %>% 
        mutate(rank = row_number())
    }) %>% 
    set_names(as_of_dates) %>% 
    bind_rows(.id = "as_of_date") %>% 
    mutate(
      as_of_date = as.Date(as_of_date),
      day = as.numeric(as_of_date - as_of_dates[1] + 1)
    )
  
  # auto-adjust the width of team label
  label_width <- 0.15 * as.numeric(max(as_of_dates) - min(as_of_dates))
  
  
  #==== plot ====
  ggplot(data = rankings, aes(x = day, y = rank, group = team)) +
    geom_line(aes(alpha = 1, color = team), size = 2) +
    geom_point(aes(alpha = 1, color = team), size = 6) +
    geom_point(color = "white", size = 2) +
    scale_x_continuous(
      breaks = seq(from = min(rankings$day), to = max(rankings$day), by = frequency), 
      minor_breaks = seq(from = min(rankings$day), to = max(rankings$day), by = frequency), 
      expand = c(.1, .1),
      labels = as_of_dates %>% format("%b %d")
    ) +
    scale_y_reverse(
      breaks = seq_along(unique(rankings$team)),
      sec.axis = dup_axis(),
      expand = c(.015, .015)
    ) +
    # the label on the left side
    geom_text(
      data = rankings %>% filter(day == min(day)),
      aes(label = team, x = min(rankings$day) - label_width) , 
      # fontface = "bold", 
      color = "black", 
      size = 5.5
    ) +
    # the label on the right side
    geom_text(
      data = rankings %>% filter(day == max(day)),
      aes(label = team, x = max(rankings$day) + label_width) ,
      # fontface = "bold",
      color = "black",
      size = 5.5
    ) +
    scale_color_manual(
      values = main_color
    ) + 
    labs(
      title = title,
      subtitle = paste0("Last updated on ", as_of_dates %>% tail(1) %>% format("%b %d, %Y"))
    ) +
    theme_bw(base_size = 20) +
    theme(
      legend.position = "none",
      # plot.title = element_text(hjust = 0.04),
      # plot.subtitle = element_text(hjust = 0.03),
      plot.margin = margin(0, 0, 0, 0, "cm"),
      axis.text.x = element_text(angle = 0),
      axis.text.y.left = element_text(size = 15, margin = margin(0, 0, 0, 0, "cm")),
      axis.text.y.right = element_text(size = 15, margin = margin(0, 0, 0, 0, "cm")),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.border = element_blank()
    )
}


#' find the mode of a matrix
#' 
#' @param x a matrix
#' 
#' @return the mode of the matrix, excluding NAs
#' 
find_mode <- function(x) {
  ux <- x %>% as.vector() %>% na.omit() %>% unique()
  ux[which.max(tabulate(match(x, ux)))]
}
