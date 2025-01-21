## code to prepare `cef_coast_4326` dataset goes here


cef_coast_4326 <- readRDS("C:/Users/madtig/Documents/repositories/SeabORD/seabord-r-dev/Data/cef_coast_4326.rds")

usethis::use_data(cef_coast_4326, overwrite = TRUE)
