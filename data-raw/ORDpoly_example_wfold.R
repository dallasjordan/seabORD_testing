## code to prepare `ORDpoly_example_wfold` dataset goes here

ORDpoly_example_wfold <- sf::read_sf("C:/Users/chrpol/Documents/density_decay_data/owfpolys.shp")

usethis::use_data(ORDpoly_example_wfold, overwrite = TRUE)
