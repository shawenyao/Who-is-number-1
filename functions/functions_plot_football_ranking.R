#' plot the evolution of football power ranking
#' 
#' @param ranking_start_date the first date of the ranking
#' @param ranking_end_date the last date of the ranking
#' @param frequency the time interval between two consecutive rankings
#' @param scoreboard the complete scoreboard data.frame
#' @param fc_logos the football club logo file list
#' @param title plot of the title
#' 
#' @return a ggplot object
#' 
plot_football_ranking <- function(
  ranking_start_date,
  ranking_end_date,
  frequency = 7,
  scoreboard,
  fc_logos,
  title
){
  
  #==== team logos ====
  fc_logo_img_list <- paste0("icons/small/", fc_logos$id, ".png") %>% 
    map(readPNG) %>% 
    set_names(fc_logos$team)
  
  main_color <- fc_logo_img_list %>% 
    map(function(color_matrix){
      
      color <- rgb(color_matrix[,,1], color_matrix[,,2], color_matrix[,,3])
      
      # exclude white and black color
      color[color == "#FFFFFF" | color == "#000000"] <- NA
      
      # find the most popular color in the logo
      find_mode(color)
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
          scoreboard = scoreboard %>% 
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
  
  
  #==== plot ====
  # auto-adjust the scale based on the range of x axis
  scale_multiplier <- as.numeric(max(as_of_dates) - min(as_of_dates))
  
  output_plot <- ggplot(data = rankings, aes(x = day, y = rank, group = team)) +
    
    # the geoms
    geom_line(aes(color = team, alpha = day), size = 2) +
    geom_point(aes(color = team, alpha = day), size = 6) +
    geom_point(color = "white", size = 2) +
    
    # fromat transparency
    scale_alpha_continuous(range = c(0.2, 0.9)) + 
    
    # format x axis
    scale_x_continuous(
      breaks = seq(from = min(rankings$day), to = max(rankings$day), by = frequency), 
      minor_breaks = seq(from = min(rankings$day), to = max(rankings$day), by = frequency), 
      expand = c(.114, .114),
      labels = as_of_dates %>% format("%b %d")
    ) +
    
    # format y axis
    scale_y_reverse(
      breaks = seq_along(unique(rankings$team)),
      sec.axis = dup_axis(),
      expand = c(.015, .015)
    ) +
    
    # the label on the left side
    geom_text(
      data = rankings %>% filter(day == min(day)),
      aes(label = team, x = min(rankings$day) - 0.125 * scale_multiplier), 
      # fontface = "bold", 
      color = "black", 
      size = 5.2
    ) +
    
    # the label on the right side
    geom_text(
      data = rankings %>% filter(day == max(day)),
      aes(label = team, x = max(rankings$day) + 0.125 * scale_multiplier),
      # fontface = "bold",
      color = "black",
      size = 5.2
    ) +
    
    # assign one color for each team
    scale_color_manual(
      values = main_color
    ) + 
    
    # the title
    labs(
      title = title,
      subtitle = paste0("Last updated on ", as_of_dates %>% tail(1) %>% format("%b %d, %Y"))
    ) +
    
    # other aesthetics
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
  
  
  #==== add team logo to the plot ====
  # loop over each team
  for(i in seq_along(unique(rankings$team))){
    
    output_plot <- output_plot +
      
      # the team logo on the left side
      annotation_custom(
        grob = rasterGrob(
          fc_logo_img_list[[rankings %>% filter(day == min(day)) %>% pull(team) %>% extract(i)]],
          interpolate = FALSE
        ),
        xmin = min(rankings$day) - 0.25 * scale_multiplier - 0.04 * scale_multiplier / 2,
        xmax = min(rankings$day) - 0.25 * scale_multiplier + 0.04 * scale_multiplier / 2,
        ymax = 99.9 - i * 2 - 0.83 / 2
      ) +
      
      # the team logo on the right side
      annotation_custom(
        grob = rasterGrob(
          fc_logo_img_list[[rankings %>% filter(day == max(day)) %>% pull(team) %>% extract(i)]],
          interpolate = FALSE
        ),
        xmin = max(rankings$day) + 0.25 * scale_multiplier - 0.04 * scale_multiplier / 2,
        xmax = max(rankings$day) + 0.25 * scale_multiplier + 0.04 * scale_multiplier / 2,
        ymax = 99.9 - i * 2 - 0.83 / 2
      )
  }
  
  output_plot
}


#' find the mode of a matrix
#' 
#' @param x a vector
#' 
#' @return the mode of the vector, excluding NAs
#' 
find_mode <- function(x) {
  
  ux <- x %>% na.omit() %>% unique()
  ux[which.max(tabulate(match(x, ux)))]
}
