## code to prepare `example_lists_dd` dataset goes here


########################################################
##################### EXAMPLE 1 LISTs ##################
########################################################

####################################################
############## switches ############################
####################################################

# Control switches:
switches_example <- list(
  environment = "serial",
  modelmode = "scenario",
  debugmode = 0,
  bycol = FALSE,
  bysus = FALSE,
  bych = FALSE,
  printdaily = FALSE,
  printseason = FALSE,
  printpair = FALSE,
  printfinal = FALSE,
  minout = FALSE,
  silent = FALSE,
  saverds = FALSE,
  savebirdflightmap = FALSE
)

####################################################
##################### Par ##########################
####################################################

Par_example <- list(
  thisSpecies = "KI",
  colonies = "UK9002491",
  Nscalefactor = 1,
  Prob_Displacement = 0.6,
  Prob_Barrier = 1,
  PreyType = "Uniform",
  collision = "Off",
  SiteSelectionMethod = "Map",
  MaxDistancekm = 0,
  PropInRange = 0,
  Npairspercol = 2898,
  Pmedian = 200
)


####################################################
##################### modPar #######################
####################################################

modPar_example <- list(
  Nparallel = NA, #only for running in parallel - not the case for this example
  initialseed = 6598, #to ensure reproducibility
  reference =  "serial_scenario_KI_UK9002491",
  outputdir = "output_seabORD",
  Nreplicates = 1 #number of reps so prey values can be inferred in given range
)


####################################################
##################### ordPar #######################
####################################################

ordPar_example <- list(
  include_ORDs = c("Hywind Scotland Pilot Park", "Aberdeen Offshore W/F","Kincardine"),
  parnames     =  "HYWD;ABDN;KINC",
  FootprintBorder = 2, #km
  BufferZone = 5 #km
)

#######################################################################
######################## build a list of lists #######################
#######################################################################
example_lists_dd <-
  list(
    switches = switches_example,
    Par = Par_example,
    modPar = modPar_example,
    ordPar = ordPar_example
  )

usethis::use_data(example_lists_dd, overwrite = TRUE)
