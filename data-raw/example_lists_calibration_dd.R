## code to prepare `example_lists_calibration_dd` dataset goes here

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
  colonies = "UK9002491",
  Nscalefactor = 0.1,
  Prob_Displacement = 0.6,
  Prob_Barrier = 1,
  PreyType = "Uniform",
  collision = "Off",
  SiteSelectionMethod = "Map",
  MaxDistancekm = 0,
  PropInRange = 0,
  Npairspercol = 1350,
  Pmedian = c(190.0000, 191.0526, 192.1053, 193.1579, 194.2105, 195.2632,
              196.3158, 197.3684, 198.4211, 199.4737, 200.5263, 201.5789, 202.6316, 203.6842,
              204.7368, 205.7895, 206.8421, 207.8947, 208.9474, 210.0000)
)




####################################################
##################### modPar #######################
####################################################

modPar_example <- list(
  Nparallel = NA, #only for running in parallel - not the case for this example
  initialseed = 6598, #to ensure reproducibility
  reference =  "serial_calibration_KI_UK9002491",
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
example_lists_calibration_dd <-
  list(
    switches = switches_example,
    Par = Par_example,
    modPar = modPar_example,
    ordPar = ordPar_example
  )

usethis::use_data(example_lists_calibration_dd, overwrite = TRUE)
