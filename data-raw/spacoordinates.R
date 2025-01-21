## code to prepare `spacoordinates` dataset goes here

spacoordinates <- read.csv("C:/Users/madtig/Documents/repositories/SeabORD/seabord-r-dev/Data/spacoordinates.csv")



usethis::use_data(spacoordinates, overwrite = TRUE)
