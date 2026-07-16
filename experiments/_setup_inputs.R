################################################################################
## Shared setup for all seabORD experiments.
##
## Source this at the top of an experiment script:
##   source("experiments/_setup_inputs.R")
##
## It loads the package + helpers, builds every shared input (rasters, colony
## tibbles, species data), and exposes the base parameter lists and the
## CALIBRATED prey level. Experiment scripts then only define their own
## windfarms / scenarios / analysis -- so shared inputs live in ONE place and
## the scripts never drift out of sync.
##
## Objects created for the caller:
##   seamask, BrdData, FrgCompData, fltdist_base   (RasterLayers / brick)
##   spadat1, spadat2, spdat                        (tibbles)
##   FlightGridcorrection
##   Par, modPar, ordPar, switches                  (base lists, unmodified)
##   CALIBRATED_PMEDIAN                             (from experiment 03)
##   COLONY_XY                                      (Isle of May, EPSG:3035)
##   SPA_CODE, WINDFARM_SHP
##   helper functions from transect_helpers.R and windfarms.R
################################################################################

.libPaths(c("C:/Users/dallas.jordan/AppData/Local/R/win-library/4.6", .libPaths()))
suppressPackageStartupMessages({
  library(seabORD); library(raster); library(sf); library(dplyr)
  library(purrr); library(tibble)
})
setwd("C:/Users/dallas.jordan/OneDrive - SLR Consulting/Projects/seabORD_testing")
dir.create("outputs", showWarnings = FALSE)

source("experiments/transect_helpers.R")
source("experiments/windfarms.R")

# ---- Calibrated baseline prey (experiment 03; moderate Isle of May conditions) ----
# 175 g/cell: the only value in the refined 165-185 sweep with BOTH adult mass
# loss (9-11%) and chicks/nest (0.45-0.55) inside their moderate bounds.
# Chicks/nest is the binding constraint -- adult mass loss is satisfied across a
# much wider prey range. Re-run 03_calibrate_pmedian.R if the inputs change.
CALIBRATED_PMEDIAN <- 175   # g/cell

# ---- Paths / codes ----
SPA_CODE     <- "UK9004171"                                   # Forth Islands SPA
WINDFARM_SHP <- "data/ShapefilesForSeabORD/WindfarmsForSeabORD.shp"

# ---- Load package datasets ----
data("example_1_lists");        data("seamask_3035_example"); data("BrdData_example")
data("frgcompdata_example");    data("UK9004171_bysea_3035"); data("FlightGridcorrection_3035")
data("energeticsandpreydata");  data("spacoordinates");       data("spalist")

rebuild_raster <- function(rlist, name = NULL) {
  md <- rlist$metadata
  r <- raster::setValues(raster::raster(nrows = md$n_rows, ncols = md$n_cols,
        xmn = md$x_min, xmx = md$x_max, ymn = md$y_min, ymx = md$y_max, crs = md$crs),
        rlist$matrix)
  if (!is.null(name)) names(r) <- name
  r
}

# ---- Spatial + tabular inputs ----
seamask      <- rebuild_raster(seamask_3035_example, "seamask_3035")
BrdData      <- rebuild_raster(BrdData_example,      "Forth.Islands")
FrgCompData  <- rebuild_raster(frgcompdata_example,  "Forth.Islands")
fltdist_base <- raster::brick(rebuild_raster(UK9004171_bysea_3035, SPA_CODE))
spadat1      <- tibble::as_tibble(dplyr::filter(spacoordinates, SITECODE  == SPA_CODE))
spadat2      <- tibble::as_tibble(dplyr::filter(spalist,        SITE_CODE == SPA_CODE))
spdat        <- dplyr::filter(energeticsandpreydata, Code == "KI")
FlightGridcorrection <- FlightGridcorrection_3035
names(BrdData) <- paste(SPA_CODE, "KI", sep = "_")

# Colony location (Isle of May) in the seamask CRS, for "offal near the colony".
COLONY_XY    <- c(spadat1$fltxy.E, spadat1$fltxy.N)
COLONY_POINT <- sf::st_sfc(sf::st_point(COLONY_XY), crs = sf::st_crs(seamask))

# ---- Base parameter lists (leave unmodified; experiments customise copies) ----
Par      <- example_1_lists$Par
modPar   <- example_1_lists$modPar
ordPar   <- example_1_lists$ordPar
switches <- example_1_lists$switches

message("Setup loaded: calibrated Pmedian = ", CALIBRATED_PMEDIAN,
        " g/cell; colony at (", round(COLONY_XY[1]), ", ", round(COLONY_XY[2]), ").")
