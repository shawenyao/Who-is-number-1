suppressWarnings(library(rio))


#==== football club list ====
setwd("C:/Users/Wenyao/Desktop/R/Who-is-number-1/data")
fc <- import("european_football_clubs.csv")


#==== small logos ====
setwd("C:/Users/Wenyao/Desktop/R/Who-is-number-1/icons/small")

for(i in 1:nrow(fc)){
  print(i)
  
  try(
    download.file(
      url = paste0("https://a.espncdn.com/combiner/i?img=/i/teamlogos/soccer/500/", fc$id[i], ".png&h=50"),
      destfile = paste0(fc$id[i], ".png"),
      quite = TRUE,
      method = "curl"
    )
  )
}


#==== large logos ====
setwd("C:/Users/Wenyao/Desktop/R/Who-is-number-1/icons/large")

for(i in 1:nrow(fc)){
  print(i)
  
  try(
    download.file(
      url = paste0("https://a.espncdn.com/combiner/i?img=/i/teamlogos/soccer/500/", fc$id[i], ".png&h=500"),
      destfile = paste0(fc$id[i], ".png"),
      quite = TRUE,
      method = "curl"
    )
  )
}
