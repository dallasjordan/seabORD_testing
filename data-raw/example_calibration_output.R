## code to prepare `example_calibration_output` dataset goes here

example_calibration_output <- readRDS("C:/Users/madtig/Documents/repositories/SeabORD/documents/sb_out_serial_calibration_KI_UK9004171_2025_01_15.rds")

usethis::use_data(example_calibration_output, overwrite = TRUE)
