## code to prepare `CEF_colsize_table` dataset goes here

CEF_colsize_table <- read.csv("C:/Users/madtig/Documents/repositories/SeabORD/seabord-r-dev/Data/CEF_colsize_table.csv")

usethis::use_data(CEF_colsize_table, overwrite = TRUE)
