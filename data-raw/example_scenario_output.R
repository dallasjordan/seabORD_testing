## code to prepare `example_scenario_output` dataset goes here

example_scenario_output <- readRDS("C:/Users/madtig/Documents/repositories/SeabORD/documents/sb_out_serial_scenario_KI_UK9004171_2025_01_29.rds")


usethis::use_data(example_scenario_output, overwrite = TRUE)
