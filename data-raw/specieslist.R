
## code to prepare `specieslist` dataset goes here

#get the data from repositiry
specieslist <- read.csv("C:/Users/madtig/Documents/repositories/SeabORD/seabord-r-dev/Data/specieslist.csv")

#save it in the package
usethis::use_data(specieslist, overwrite = TRUE)
