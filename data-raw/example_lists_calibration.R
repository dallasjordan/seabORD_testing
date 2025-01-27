
########################################################
##################### EXAMPLE 1 LISTs ##################
########################################################

####################################################
############## switches ############################
####################################################

# Control switches:
switches_example <- list(
  environment = "serial",
  modelmode = "calibration",
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
  saverds = TRUE,
  savebirdflightmap = FALSE
)

####################################################
##################### Par ##########################
####################################################

Par_example <- list(
  thisSpecies = "KI",
  colonies = "UK9004171",
  Nscalefactor = 0.1,
  Prob_Displacement = 0.6,
  Prob_Barrier = 1,
  PreyType = "Uniform",
  collision = "Off",
  SiteSelectionMethod = "Map",
  MaxDistancekm = 0,
  PropInRange = 0,
  Npairspercol = 2898,
  Pmedian = c(170.0000,170.5263, 171.0526, 171.5789, 172.1053, 172.6316,173.1579, 173.6842,
              174.2105 ,174.7368, 175.2632, 175.7895, 176.3158, 176.8421,
              177.3684, 177.8947, 178.4211, 178.9474, 179.4737, 180.0000)
)


####################################################
##################### modPar #######################
####################################################

modPar_example <- list(
  Nparallel = NA, #only for running in parallel - not the case for this example
  initialseed = 6598, #to ensure reproducibility
  reference =  "serial_calibration_KI_UK9004171",
  outputdir = "output_seabORD",
  Nreplicates = 20 #number of reps so prey values can be inferred in given range
)


####################################################
##################### ordPar #######################
####################################################

ordPar_example <- list(
)

#######################################################################
######################## build a list of lists #######################
#######################################################################
example_lists_calibration <-
  list(
    switches = switches_example,
    Par = Par_example,
    modPar = modPar_example,
    ordPar = ordPar_example
  )

usethis::use_data(example_lists_calibration, overwrite = TRUE)
