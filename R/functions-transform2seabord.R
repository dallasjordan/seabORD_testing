####################################################################################################
## FUNCTIONS for taking data from the CEF Data Store and writing to a format
##   expected by SeabORD v2
## Author: Deena C. Mobbs
## Date: May 2022
##

## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#' @title Creating colony list
#'
#' @description This function takes data in the format pulled from the CEF Data Store by the default
#'   read functions and transforms it into a comma-separated values file compatible with SeabORD
#'   v2.0.
#'
#' @param n Vector of number of species pairs per SPA (Par$Npairspercol)
#' @param f Scale factor for population size (Par$Nscalefactor)
#' @param spadat1 Tibble; output from read_spacoords(sitecode = Par$colonies)
#' @param spadat2 Tibble; output from read_spalist(code = Par$colonies)
#'
#' @importFrom tibble tibble
#' @importFrom tidyr tribble
#'
#' @return A list containing the human-friendly metadata and the code-friendly
#'   data tibbles
#'
#' @noRd
transform_sbcolony <- function(n, f = 1, spadat1, spadat2) {

  ColonyData <- tibble::tibble(
    code = spadat1$SITECODE,
    Colony = spadat2$SITE_NAME,
    Geometry = "Point",
    Easting = spadat1$fltxy.E,
    Northing = spadat1$fltxy.N,
    long = spadat1$flt.LONG,
    lat = spadat1$flt.LAT,
    ObsNpairs = n
  ) %>%
    dplyr::mutate(across(contains("ObsNpairs"),
                         function(a) {ceiling(a * f)}))

  # See local SeabORD... this should be read from Data Library ##*unfinished
  ColonyDataTable <- tidyr::tribble(
    ~VarName, ~VarDescription,
    "code","Unique code to identify the colony",
    "Colony","Full name of the colony (Special Protection Area)",
    "Geometry","How is this colony described, point or polygon?",
    "Easting","Easting (EPSG 3035)",
    "Northing","Northing (EPSG 3035)",
    "long","Longitude values (decimal)",
    "lat","Latitude values (decimal)",
    "ObsNpairs", "Number of pairs of birds"
  )

  return(list("data" = ColonyData, "metadata" = ColonyDataTable))

}

## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#' @title Creating species parameter list
#'
#' @description This function takes data in the format pulled from the CEF Data Store by the default
#'   read functions and transforms it into a comma-separated values file compatible with SeabORD
#'   v2.0.
#'
#' @param sdat Tibble; output from read_energeticsprey(spcode = Par$thisSpecies)
#'
#' @importFrom tibble tibble
#' @importFrom tidyr tribble
#' @importFrom magrittr %>%
#' @importFrom dplyr rename
#'
#' @return A list containing the human-friendly metadata and the code-friendly
#'   data tibbles
#'
#' @noRd
transform_sbspecies <- function(sdat) {

  SpeciesData <- sdat %>%
    dplyr::rename(SID = Code) %>%
    dplyr::rename(Name = Species)

  # See local SeabORD... this should be read from Data Library ##*unfinished
  SpeciesDataTable <- tidyr::tribble(
    ~VarName, ~VarDescription, ~VarUnits,
    "SID","Species two-letter code","",
    "Name","Species name","",
    "BM_adult_mn","Initial adult body mass mean","g",
    "BM_adult_sd","Initial adult body mass standard deviation","sd",
    "BM_adult_mortf","Critical mass below which adult is assumed dead","proportion of mean mass",
    "BM_adult_abdn","Critical mass below which adult abandons chick","proportion of mean mass",
    "BM_chick_mn","Initial chick body mass mean","g",
    "BM_chick_sd","Initial chick body mass standard deviation ","g",
    "BM_Chick_mortf","Critical mass below which chick is dead","proportion of initial mass",
    "daylength","Number of hours per timestep","hours",
    "seasonlength","Number of timesteps per season","",
    "unattend_max_hrs","Critical time threshold for unattendance at nest above which a chick is assumed to die through exposure or predation","hours",
    "adult_DEE_mn","Adult daily energy expenditure mean","kJ",
    "adult_DEE_sd","Adult daily energy expenditure standard deviation","kJ",
    "chick_DER","Chick energy requirement, Harris & Wanless 1985","kJ per day",
    "IR_max","Maximum prey intake rate","g per minute",
    "IR_half_a"," Intake rate parameter","",
    "IR_half_b","Intake rate parameter","",
    "flight_msec","Average speed in flight","metre per second",
    "pelagic","Fraction of dives assumed to be pelagic (not to sea bed)","",
    "forage_depth_mn","Diving depth mean (set to 0 for non diving species)","m",
    "forage_depth_sd","Diving depth standard deviation (set to 0 for non diving species)","0",
    "assim_eff","Assimilation efficiency, Hilton et al 2000b","",
    "energy_prey","Energy gained from prey, Harris et al 2008","kJ per gram",
    "energy_nest","Energy cost of nesting at colony","kJ per day",
    "energy_flight","Energy cost of flight","kJ per day",
    "energy_searest","Energy cost of resting at sea","kJ per day",
    "energy_forage","Energy cost of foraging","kJ per day",
    "energy_warming","Energy cost of warming food","kJ per day",
    "chick_mass_a","Maximum chick mass gain per day","g",
    "adult_mass_KG","Energy density of the adult bird tissue","kJ per gram",
    "beta","Mass-survival slope (parameter for mass-survival relationship)","",
    "basesurv_poor","Baseline survival (parameter for mass-survival relationship) in poor years","",
    "basesurv_modr","Baseline survival (parameter for mass-survival relationship) in moderate years","",
    "basesurv_good","Baseline survival (parameter for mass-survival relationship) in good years","",
    "massloss_poor","Expected typical adult body mass % loss for poor years","%",
    "massloss_modr","Expected typical adult body mass % loss for moderate years","%",
    "massloss_good","Expected typical adult body mass % loss for good years","%",
    "chicksurv_poor","Expected typical chick survival % for poor years","%",
    "chicksurv_modr","Expected typical chick survival % for moderate years","%",
    "chicksurv_good","Expected typical chick survival % for good years","%"
  )

  return(list("data" = SpeciesData, "metadata" = SpeciesDataTable))

}
