#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#     Functions for distance decay maps    #
#       written by Adam Butler (BioSS)     #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Adam's code ------
# from "/data/notebooks/rstudio-ceframework/ceframework/R/function-calcfixedinputdata.R"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Calculate distance restricted by obstructions
#' @details Grid cells are treated as obstructed if (a) they have a missing (NA) value in the map, or, if "obspolys" has been provided, the grid cell lies within any of the polygons in "obspolys". Created 26 May 2024 based on "make_fltdist_scen" in SeabORD, but (a) for a single colony and (b) for obstruction either by land or other (e.g. footprints)
#' @param mymap A raster in which grid cells on land are assumed to have missing (NA) values
#' @param obspolys Polygons associated with obstacles; if NULL there are assumed to be no obstacles other than land
#' @param FlightGridcorrection_3035 The 'flight correction' layer required by gdistance to correct for latitude (see gdistance)
#' @param obspenalty Penalty value associated with crossing an obstruction; a positive number
#' @param maxdist Maximum distance, in km; above this value are fixed to zero
#' @importFrom gdistance transition
#' @importFrom raster xyFromCell calc
#' @return Raster, containing distance to target from each grid, avoiding obstructions, in kilometres
#' @export

calc_dist_restricted <- function(mymap, obspolys=NULL, targetcoords,
                                 FlightGridcorrection_3035, directions=16,
                                 obspenalty=1e+08, maxdist=9000){

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Step 1. Set map values to be NA if polygons provided and grid cells are within polygons

  if(! is.null(obspolys)){

    polys_stc <- sf::st_sf(geom = sf::st_as_sfc(obspolys, crs = "EPSG:3035")) ## corrected 2 June 2024: st_as_sfc not st_sfc

    polys_whichcells <- unlist(lapply(raster::extract(mymap, polys_stc, cellnumbers=TRUE), function(x){x[,1]}))

    raster::values(mymap)[unique(polys_whichcells)] <- NA
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Step 2. Penalty for crossing each grid cell: one for sea, "obspenalty" for land
  cellpenalty <-
    raster::calc(mymap, fun = function(r) { r[!is.na(r)] <- 1 ; r[is.na(r)] <- obspenalty; return(r) })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Step 3. Make the TransitionLayer with new obstructions
  FlightTL <- gdistance::transition(
    cellpenalty,
    transitionFunction = function(x) 1/mean(x),
    directions = directions)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Step 4. Apply the standard CEF correction factor
  FlightcorrectedTL <- FlightTL * FlightGridcorrection_3035

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Step 5. Unrestricted grid cells
  unres_whichcells <- which(! is.na(mymap[]))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Step 6. Coordinates of grid cells
  availablecoords <- raster::xyFromCell(mymap, unres_whichcells)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Step 7. Calculate cost distance (EPSG:3035)
  targetthis <- sf::st_coordinates(targetcoords)

  costdist_metres <- gdistance::costDistance(x = FlightcorrectedTL,
                                             fromCoords = targetthis,
                                             toCoords = availablecoords)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Step 8. Convert cost distance from metres to km
  costdist_km <- 0.001 * costdist_metres

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Step 9. Restricted distance
  resdist <- raster::raster(mymap)
  raster::values(resdist) <- NA
  resdist[unres_whichcells] <- costdist_km
  resdist[resdist > maxdist] <- 0

  resdist[] <- as.numeric(resdist[])

  resdist
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Added 26 Jan 2023
#' @title Calculate bird density maps
#' @description Calculate bird density maps using the distance-decay function used in SeabORD, via users specifying "proportion of UD lying within the foraging range"
#' @param dmap A raster containing the distance by sea from each grid cell to the population of interest
#' @param fr Foraging range, in kilometres
#' @param pinfr The proportion of the UD that is assumed to lie within a distance `fr` of the population: a numeric value between 0 and 1
#' @return A raster containing the distance-decay map
#' ## 20 Oct 2023: added "dmin" argument
#' @export
calc_birddensmap_dd_pinfr <- function(dmap, fr, pinfr, dmin=1){

  if(mode(dmap) == "logical"){ ## added 20 Oct 2023

    out <- NA ## added 20 Oct 2023
  }
  else{

    d <- terra::values(dmap)

    d[d < dmin] <- NA ## added 20 Oct 2023

    u <- (((1-pinfr)^(d/fr)) / d)

    u <- u * (d <= fr)

    out <- dmap

    terra::values(out) <- (u / sum(u, na.rm=TRUE))
  }

  out
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Added 26 Jan 2023
#' @title Calculate bird density maps used the distance-decay function used in SeabORD, via users specifying `phalf`
#' @inheritParams calc_birddensmap_dd_pinfr
#' @param pinhalf the proportion of the foraging range within which half of the UD lies
#' @return A raster containing the distance-decay map
#' @export
calc_birddensmap_dd_pinhalf <- function(dmap, fr, pinhalf){

  pinfr <- calc_dd_pinfr_from_phalf(pinhalf) ## 20 Oct 2023: moved to separate row to make debugging easier

  calc_birddensmap_dd_pinfr(dmap = dmap, fr = fr, pinfr = pinfr)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Calculate 'q' from 'p' for the distance-decay model
#' @details "pinfr": the probability of being within the foraging range; "phalf": the proportion of the foraging range within which half of the UD lies
#' Mathematical derivation:
#' Define q to be such that: `[1 - exp(-beta * fr * phalf)] / [1 - exp(-beta * fr)] = 1/2`
#' Since: `beta = −log(1 − pinfr) / fr`
#' We can immediately derive: `phalf = log(1 - (pinfr/2)) / log(1 - pinfr)`
#' @param pinfr A vector of numeric values containing the values of 'pinfr'
#' @return A vector of length `length(pinfr)` containing the values of `phalf` associated with `pinfr`
#' @export
calc_dd_phalf_from_pinfr <- function(pinfr){log(1-(pinfr/2)) / log(1-pinfr)}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Calculate 'pinfr' from 'phalf' for the distance-decay model
#' @details "pinfr": the probability of being within the foraging range; "phalf": the proportion of the UD within the foraging range that is within half the foraging range
#' This is optimized numerically
#' @param phalf A vector of numeric values containing the values of 'phalf'
#' @return A vector of length `length(q)` containing the values of `pinfr` associated with `phalf`
#' @export
calc_dd_pinfr_from_phalf <- function(phalf){ voptimise(y = phalf, fny = calc_dd_phalf_from_pinfr, iv = c(0,1)) }

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Use numerical optimisation to give the value of `x` associated with each element of a vector `y`, where `y = fny(x)` for a specified function `fny`
#' @param y A vector of numerical values
#' @param fny A function with a single argument `x` that produces the value of `y` associated with `x`
#' @param iv A vector of length two indicating the numerical range of values to optimize over
#' @return A vector of numeric values of lengt `y` containing the value of `x` associated with each value of `y`
#' @export
voptimise <- function(y, fny, iv){

  optfn <- function(x,y,fny){ abs(y - fny(x)) }

  out <- rep(NA, length(y))

  for(k in 1:length(y)){

    out[k] <- optimise(optfn, interval = iv, y = y[k], fny = fny)$minimum
  }

  out
}


