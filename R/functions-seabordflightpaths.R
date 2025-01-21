####################################################################################################
## FUNCTIONS for creating distance by sea raster
## Author: UKCEH
## Date: From August 2022
##

## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#' @title Creating the TransitionLayers for use by distance function
#'
#' @description This function takes the ORD footprint(s) and make a new TransitionLayer
#'
#' @param Colony List of SPAs used (V1.0 allowed for multiple SPAs. V2.0 only ever has one)
#' @param inORDborder Raster grid cell numbers that are within the footprint border
#' @param ordPar The ORD footprint list
#' @param seamask A raster indicating which cells are sea (0) and which are land (NA)
#' @param FlightGridcorrection_3035 The 'flight correction' layer required by gdistance to correct
#'   for latitude (see gdistance)
#'
#' @importFrom gdistance transition
#' @importFrom raster xyFromCell calc brick
#'
#' @return A RasterBrick with one layer per colony
#' @noRd

make_fltdist_scen <- function(Colony, inORDborder, ordPar, seamask, FlightGridcorrection_3035){

  # Make the ORD cells an obstruction
  ORDmask <- seamask
  ORDmask[unlist(inORDborder)] <- NA

  # Make the TransitionLayer with new obstructions
  obsnum <- 1e+08;
  f <- function(x) 1/mean(x)
  ORDblock <- ORDmask %>%
    calc(fun = function(r) {r[r==0] <- 1; return(r)}) %>%
    calc(fun = function(r) {r[is.na(r)] <- obsnum; return(r)})

  # TransitionLayer
  FlightTL <- gdistance::transition(ORDblock,
                                    transitionFunction = f,
                                    directions = 16)

  # Apply the standard CEF correction factor
  FlightTL <- FlightTL * FlightGridcorrection_3035

  # Check
  # p4 <- ggplot() + ggspatial::layer_spatial(raster(FlightTL)) +
  #     scale_fill_viridis_c(option = "A", direction = 1, na.value="transparent") +
  #     ggtitle("Corrected conductance (EPSG:3035)") +  ylim(3700000, 4000000) + xlim(3300000, 3700000)
  # p4

  # toCoords
  seacells <- raster::xyFromCell(ORDmask, which(ORDmask[] == 0))

  # Generate the Rasters
  fltdist_scen <- list()
  for (i in 1:nrow(Colony$data@data)){

    # cost distance EPSG:3035
    costdist <- gdistance::costDistance(x = FlightTL,
                                        fromCoords = Colony$data@data$StartCoords[i,],
                                        toCoords = seacells)
    bysea <- seamask; values(bysea) <- NA
    bysea[which(ORDmask[] == 0)] <- 0.001 * costdist
    bysea[bysea > 9000] <- 0
    fltdist_scen[[i]] <- bysea
  }

  # Make a Brick.
  fltdist_scen <- fltdist_scen %>% brick()
  names(fltdist_scen) <- Colony$data@data$code

  # Check
  # p6 <- ggplot() + ggspatial::layer_spatial(fltdist_scen[[2]] - fltdist_base[[2]]) +
  #    scale_fill_viridis_c(option = "D", direction = -1, na.value = "transparent") +
  #    ggtitle("Distance, km") #+  ylim(3600000, 4100000) + xlim(3300000, 3700000)
  # p6

  return (fltdist_scen)

}
