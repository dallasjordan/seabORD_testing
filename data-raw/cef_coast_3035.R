## code to prepare `cef_coast_3035` dataset goes here


cef_coast_3035 <- readRDS("C:/Users/chrpol/Documents/density_decay_data/cef_coast_3035.rds")

usethis::use_data(cef_coast_3035, overwrite = TRUE)
