################################################################################
## Shared setup for all seabORD experiments.
##
##   source("experiments/_setup_inputs.R")
##
## Edit study-wide inputs HERE, not in the experiment scripts. Everything below
## the USER INPUTS block is machinery.
##
## Provides:
##   seamask, BrdData, FrgCompData, fltdist_base, FlightGridcorrection  (rasters)
##   spadat1, spadat2, spdat                                           (tibbles)
##   Par, modPar, ordPar, switches                                     (base lists)
##   COLONY_PAIRS, POP_FRACTION, CALIBRATED_PMEDIAN, COLONY_POINT
##   SPA_CODE, WINDFARM_SHP
##   helpers from transect_helpers.R and windfarms.R
################################################################################

# =============================================================================
# USER INPUTS
# =============================================================================

# --- Colony size (breeding pairs) ---------------------------------------------
# Isle of May black-legged kittiwake, 6,068 apparently occupied nests in 2025
# (NatureScot, Isle of May NNR Annual Report 2025).
#
# Drives BOTH the simulated population and the share of the colony a given offal
# deposit can feed, so results are not comparable across different values. The
# package default of 2898 is a stale example value -- do not fall back to it.
#
# Recent counts if you need a different basis: 2023 5,425 | 2024 5,443 |
# 2025 6,068 | 5-year mean 5,252.
COLONY_PAIRS <- 6068

# --- Simulated fraction of that colony ----------------------------------------
# 0.1 => 607 pairs / 1214 adults at COLONY_PAIRS = 6068. Runtime scales with it.
POP_FRACTION <- 0.1

# --- Baseline prey density (g/cell) -------------------------------------------
# From experiment 01. The only value in the 165-185 sweep with BOTH adult mass
# loss (9-11%) and chicks/nest (0.45-0.55) inside their moderate bounds.
# Chicks/nest is the binding constraint. Re-run 03 if inputs change.
CALIBRATED_PMEDIAN <- 175

# --- Displacement (NatureScot guidance) ---------------------------------------
# Set study-wide so it cannot drift between scripts. The package ships 60%,
# a demo value rather than guidance.
PROB_DISPLACEMENT <- 0.30   # share of birds that avoid windfarms
FOOTPRINT_BORDER  <- 2      # km buffer added to the footprint
PROB_BARRIER      <- 1.0    # share of DISPLACED birds that also detour around it
# PROB_BARRIER drives the extra flight cost, so it scales Berwick Bank's impact
# and therefore the whole answer. 1.0 means every displaced bird also detours --
# the most precautionary reading. Set explicitly here rather than inherited so it
# is a stated assumption.

# --- Paths and codes ----------------------------------------------------------
SPA_CODE     <- "UK9004171"   # Forth Islands SPA
WINDFARM_SHP <- "data/ShapefilesForSeabORD/WindfarmsForSeabORD.shp"

PROJECT_DIR <- "C:/Users/dallas.jordan/OneDrive - SLR Consulting/Projects/seabORD_testing"
R_LIBRARY   <- "C:/Users/dallas.jordan/AppData/Local/R/win-library/4.6"

# =============================================================================
# Environment
# =============================================================================
.libPaths(c(R_LIBRARY, .libPaths()))
suppressPackageStartupMessages({
  library(seabORD); library(raster); library(sf); library(dplyr)
  library(purrr); library(tibble)
})
setwd(PROJECT_DIR)
dir.create("outputs", showWarnings = FALSE)

source("experiments/transect_helpers.R")
source("experiments/windfarms.R")

# =============================================================================
# Package datasets
# =============================================================================
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

# =============================================================================
# Spatial and tabular inputs
# =============================================================================
seamask      <- rebuild_raster(seamask_3035_example, "seamask_3035")
BrdData      <- rebuild_raster(BrdData_example,      "Forth.Islands")
FrgCompData  <- rebuild_raster(frgcompdata_example,  "Forth.Islands")
fltdist_base <- raster::brick(rebuild_raster(UK9004171_bysea_3035, SPA_CODE))
spadat1      <- tibble::as_tibble(dplyr::filter(spacoordinates, SITECODE  == SPA_CODE))
spadat2      <- tibble::as_tibble(dplyr::filter(spalist,        SITE_CODE == SPA_CODE))
spdat        <- dplyr::filter(energeticsandpreydata, Code == "KI")
FlightGridcorrection <- FlightGridcorrection_3035
names(BrdData) <- paste(SPA_CODE, "KI", sep = "_")

# Colony location (Isle of May) in the seamask CRS.
COLONY_XY    <- c(spadat1$fltxy.E, spadat1$fltxy.N)
COLONY_POINT <- sf::st_sfc(sf::st_point(COLONY_XY), crs = sf::st_crs(seamask))

# =============================================================================
# Base parameter lists (experiments customise copies of these)
# =============================================================================
Par      <- example_1_lists$Par
modPar   <- example_1_lists$modPar
ordPar   <- example_1_lists$ordPar
switches <- example_1_lists$switches

Par$Npairspercol       <- COLONY_PAIRS
Par$Nscalefactor       <- POP_FRACTION
Par$Prob_Displacement  <- PROB_DISPLACEMENT
Par$Prob_Barrier       <- PROB_BARRIER
ordPar$FootprintBorder <- FOOTPRINT_BORDER

N_ADULTS_REAL <- 2 * COLONY_PAIRS                          # whole colony
N_ADULTS_SIM  <- 2 * ceiling(COLONY_PAIRS * POP_FRACTION)  # simulated subsample

message(sprintf("Setup: colony %d pairs (%d adults, %d simulated at %.0f%%) | Pmedian %d g/cell",
                COLONY_PAIRS, N_ADULTS_REAL, N_ADULTS_SIM, 100 * POP_FRACTION,
                CALIBRATED_PMEDIAN))
message(sprintf("Displacement: %.0f%% of birds | barrier %.0f%% of those | %g km buffer",
                100 * Par$Prob_Displacement, 100 * Par$Prob_Barrier, ordPar$FootprintBorder))
