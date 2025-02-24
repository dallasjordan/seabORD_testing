## code to prepare `ORDpoly_example_wfold` dataset goes here

lfn <- function(x) {
  return(stringr::str_to_lower(gsub("\\(|\\)", "", x)))
}

reqnames <- c("Kincardine", "Aberdeen Offshore W/F", "Hywind Scotland Pilot Park")

ORDpoly <-  sf::read_sf("~/repositories/SeabORD/seabord-r-dev/Data/ORDs/EMODnet_HA_WindFarms_20220324.gdb",
                        layer = "EMODnet_HA_WindFarms_pg_20220324", quiet = TRUE) %>%
  dplyr::filter(stringr::str_detect(stringr::str_to_lower(COUNTRY), paste(stringr::str_to_lower(c("United Kingdom")), collapse = '|')))%>%
  dplyr::filter(stringr::str_detect(lfn(NAME), paste(lfn(reqnames), collapse = '|')))

ORDpoly <- ORDpoly %>%
  sf::st_transform(crs = seabORD::seamask_3035_example$metadata$crs)

ORDpoly_example_wfold <- ORDpoly

usethis::use_data(ORDpoly_example_wfold, overwrite = TRUE)
