library(tidyverse)
library(ggimage)


#==== general setup ====
setwd("C:/Users/Wenyao/Desktop/R/Who-is-number-1/main/2022")

# assets
country <- tribble(
  ~country, ~image,
  "Qatar", "https://flagicons.lipis.dev/flags/4x3/qa.svg",
  "Ecuador", "https://flagicons.lipis.dev/flags/4x3/ec.svg",
  "Senegal", "https://flagicons.lipis.dev/flags/4x3/sn.svg",
  "Netherlands", "https://flagicons.lipis.dev/flags/4x3/nl.svg",
  "England", "https://flagicons.lipis.dev/flags/4x3/gb-eng.svg",
  "Iran", "https://flagicons.lipis.dev/flags/4x3/ir.svg",
  "United States", "https://flagicons.lipis.dev/flags/4x3/us.svg",
  "Wales", "https://flagicons.lipis.dev/flags/4x3/gb-wls.svg",
  "Argentina", "https://flagicons.lipis.dev/flags/4x3/ar.svg",
  "Saudi Arabia", "https://flagicons.lipis.dev/flags/4x3/sa.svg",
  "Mexico", "https://flagicons.lipis.dev/flags/4x3/mx.svg",
  "Poland","https://flagicons.lipis.dev/flags/4x3/pl.svg",
  "France", "https://flagicons.lipis.dev/flags/4x3/fr.svg",
  "Australia", "https://flagicons.lipis.dev/flags/4x3/au.svg",
  "Denmark", "https://flagicons.lipis.dev/flags/4x3/dk.svg",
  "Tunisia","https://flagicons.lipis.dev/flags/4x3/tn.svg",
  "Spain", "https://flagicons.lipis.dev/flags/4x3/es.svg",
  "Costa Rica", "https://flagicons.lipis.dev/flags/4x3/cr.svg",
  "Germany", "https://flagicons.lipis.dev/flags/4x3/de.svg",
  "Japan", "https://flagicons.lipis.dev/flags/4x3/jp.svg",
  "Belgium", "https://flagicons.lipis.dev/flags/4x3/be.svg",
  "Canada", "https://flagicons.lipis.dev/flags/4x3/ca.svg",
  "Morocco", "https://flagicons.lipis.dev/flags/4x3/ma.svg",
  "Croatia","https://flagicons.lipis.dev/flags/4x3/hr.svg",
  "Brazil", "https://flagicons.lipis.dev/flags/4x3/br.svg",
  "Serbia", "https://flagicons.lipis.dev/flags/4x3/rs.svg",
  "Switzerland", "https://flagicons.lipis.dev/flags/4x3/ch.svg",
  "Cameroon","https://flagicons.lipis.dev/flags/4x3/cm.svg",
  "Portugal", "https://flagicons.lipis.dev/flags/4x3/pt.svg",
  "Ghana", "https://flagicons.lipis.dev/flags/4x3/gh.svg",
  "Uruguay", "https://flagicons.lipis.dev/flags/4x3/uy.svg",
  "South Korea", "https://flagicons.lipis.dev/flags/4x3/kr.svg"
)

# group backrgound coordinates
coord_group <- tibble(x = c(-110, 110, -110, 110, -110, 110, -110, 110), y = c(80, 80, 30, 30, -30, -30, -80, -80))
coord_round_16 <- tibble(x = c(-75, 75, -75, 75, -75, 75, -75, 75), y = c(80, 80, 30, 30, -30, -30, -80, -80))
coord_round_8 <- tibble(x = c(-50, 50, -50, 50), y = c(55, 55, -55, -55))
coord_round_4 <- tibble(x = c(-25, 25), y = c(0, 0))


#==== predictions ====
group_a <- tibble(
  country = c("Qatar", "Ecuador", "Senegal", "Netherlands"),
  x = c(-120, -100, -120, -100),
  y = c(90, 90, 70, 70),
  size = 0.06
)
group_b <- tibble(
  country = c("England", "Iran", "United States", "Wales"),
  x = c(100, 120, 100, 120),
  y = c(90, 90, 70, 70),
  size = 0.06
)
group_c <- tibble(
  country = c("Argentina", "Saudi Arabia", "Mexico", "Poland"),
  x = c(-120, -100, -120, -100),
  y = c(40, 40, 20, 20),
  size = 0.06
)
group_d <- tibble(
  country = c("France", "Australia", "Denmark", "Tunisia"),
  x = c(100, 120, 100, 120),
  y = c(40, 40, 20, 20),
  size = 0.06
)
group_e <- tibble(
  country = c("Spain", "Costa Rica", "Germany", "Japan"),
  x = c(-120, -100, -120, -100),
  y = c(-20, -20, -40, -40),
  size = 0.06
)
group_f <- tibble(
  country = c("Belgium", "Canada", "Morocco", "Croatia"),
  x = c(100, 120, 100, 120),
  y = c(-20, -20, -40, -40),
  size = 0.06
)
group_g <- tibble(
  country = c("Brazil", "Serbia", "Switzerland", "Cameroon"),
  x = c(-120, -100, -120, -100),
  y = c(-70, -70, -90, -90),
  size = 0.06
)
group_h <- tibble(
  country = c("Portugal", "Ghana", "Uruguay", "South Korea"),
  x = c(100, 120, 100, 120),
  y = c(-70, -70, -90, -90),
  size = 0.06
)

round_16 <- tibble(
  country = c(
    "Netherlands", "Senegal", "England", "United States", "Argentina", "Mexico", "France", "Denmark",
    "Spain", "Japan", "Belgium", "Morocco", "Brazil", "Cameroon", "Portugal", "Uruguay"
  ),
  x = c(-75, 75, 75, -75, -75, 75, 75, -75, -75, 75, 75, -75, -75, 75, 75, -75),
  y = c(90, 70, 90, 70, 40, 20, 40, 20, -20, -40, -20, -40, -70, -90, -70, -90),
  size = 0.06
)
round_8 <- tibble(
  country = c(
    "Netherlands", "England", "Argentina", "France", "Spain", "Belgium", "Brazil", "Portugal"
  ),
  x = c(-50, 50, -50, 50, -50, 50, -50, 50),
  y = c(80, 80, 30, 30, -30, -30, -80, -80),
  size = 0.06
)
round_4 <- tibble(
  country = c(
    "Argentina", "France", "Brazil", "Belgium"
  ),
  x = c(-25, 25, -25, 25),
  y = c(55, 55, -55, -55),
  size = 0.06
)
round_2 <- tibble(
  country = c(
    "Brazil", "France"
  ),
  x = c(-25, 25),
  y = c(0, 0),
  size = 0.06
)
round_1 <- tibble(
  country = "Brazil",
  x = 0,
  y = 0,
  size = 0.06
)

# combine all
data <- bind_rows(
  group_a,
  group_b,
  group_c,
  group_d,
  group_e,
  group_f,
  group_g,
  group_h,
  round_16,
  round_8,
  round_4,
  round_2,
  round_1
) %>% 
  left_join(country, by = "country")


#==== plot ====
plot <- data %>% 
  ggplot(
    aes(x = x, y = y)
  ) +
  # group background
  geom_tile(data = coord_group, width = 42, height = 47, fill = "gray95", color = "gray50") +
  # round of 16 background
  geom_tile(data = coord_round_16, width = 22, height = 47, fill = "gray90", color = "gray45") +
  # round of 8 background
  geom_tile(data = coord_round_8, width = 22, height = 75, fill = "gray85", color = "gray40") +
  # round of 4 background
  geom_tile(data = coord_round_4, width = 22, height = 135, fill = "gray80", color = "gray35") +
  # round of 2 (final) background
  geom_tile(data = tibble(x = 0, y = 0), width = 72, height = 25, fill = "gray75", color = "gray30") +
  # flag border 
  geom_tile(width = 18, height = 14, fill = "gray20") +
  # champion border 
  geom_tile(data = tibble(x = 0, y = 0), width = 18, height = 14, fill = "gold") +
  # flag
  geom_image(aes(image = image), asp = 1.3, size = data$size) +
  # flag text
  geom_text(aes(y = y - 9, label = country), size = 4) +
  coord_fixed(xlim = c(-130, 130), ylim = c(-100, 100)) +
  theme_void() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = "white")
  )


#==== output ====
svg(paste0("world_cup_2022.svg"), width = 13, height = 10)
print(plot)
dev.off()
