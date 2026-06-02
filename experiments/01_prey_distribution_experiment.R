################################################################################
## Experiment: effect of prey distribution on seabird productivity & survival
##
## Compares seabORD outputs across:
##   1. Baseline                  -- uniform prey at default density
##   2. Uniform density sweep     -- multiple Pmedian levels, still uniform
##   3. Spatially explicit maps   -- selected cells with elevated prey
##
## All scenarios use:
##   * The same colonies, BrdData, FrgCompData, ORDpoly, seamask, etc.
##   * The same Par$initialseed         (so stochastic variation is matched)
##   * The same Nreplicates             (so statistical comparison is fair)
##
## Only the prey distribution differs between scenarios.
################################################################################

library(seabORD)
library(raster)
library(sf)
library(dplyr)
library(purrr)
library(tibble)

setwd("C:\\Users\\dallas.jordan\\OneDrive - SLR Consulting\\Projects\\seabORD_testing\\")

# Load transect-building and PreyMap-plotting helpers
source("experiments/transect_helpers.R")

set.seed(2024)  # only affects scenario construction below; the model uses Par$initialseed


# =============================================================================
# 1. Helper: rebuild a raster from the package's list-format example data
# =============================================================================
# The package ships several rasters as list(matrix, metadata). This puts them
# back into a raster::RasterLayer.

rebuild_raster <- function(rlist, name = NULL) {
  md <- rlist$metadata
  r <- raster::raster(
    nrows = md[["n_rows"]],
    ncols = md[["n_cols"]],
    xmn   = md[["x_min"]],
    xmx   = md[["x_max"]],
    ymn   = md[["y_min"]],
    ymx   = md[["y_max"]],
    crs   = md[["crs"]]
  )
  r <- raster::setValues(r, rlist$matrix)
  if (!is.null(name)) names(r) <- name
  r
}


# =============================================================================
# 2. Load shared inputs
# =============================================================================
# TODO: swap example data for the real study-area inputs when ready.

data("example_1_lists",           package = "seabORD")
data("seamask_3035_example",      package = "seabORD")
data("BrdData_example",           package = "seabORD")
data("frgcompdata_example",       package = "seabORD")
data("ORDpoly_example",           package = "seabORD")
data("UK9004171_bysea_3035",      package = "seabORD")
data("FlightGridcorrection_3035", package = "seabORD")
data("energeticsandpreydata",     package = "seabORD")
data("spacoordinates",            package = "seabORD")
data("spalist",                   package = "seabORD")

# Parameter lists
Par      <- example_1_lists$Par
modPar   <- example_1_lists$modPar
ordPar   <- example_1_lists$ordPar
switches <- example_1_lists$switches

# The example uses one SPA. Change SPA_CODE if you want a different colony.
SPA_CODE <- "UK9004171"

# Spatial inputs. Layer names follow the vignette H_exampleKI_run.Rmd recipe --
# seabORD's internals key on these specific names in places.
seamask      <- rebuild_raster(seamask_3035_example, "seamask_3035")
BrdData      <- rebuild_raster(BrdData_example,      "Forth.Islands")
FrgCompData  <- rebuild_raster(frgcompdata_example,  "Forth.Islands")
fltdist_base <- raster::brick(rebuild_raster(UK9004171_bysea_3035, SPA_CODE))

# spadat1 / spadat2 are TIBBLES (not rasters) -- single-row subsets of the
# colony-coordinate and SPA-name tables.
spadat1 <- tibble::as_tibble(dplyr::filter(spacoordinates, SITECODE  == SPA_CODE))
spadat2 <- tibble::as_tibble(dplyr::filter(spalist,         SITE_CODE == SPA_CODE))

ORDpoly              <- ORDpoly_example
FlightGridcorrection <- FlightGridcorrection_3035

# Species-specific energetics. Subset to the species being modelled.
spdat <- dplyr::filter(energeticsandpreydata, Code == Par$thisSpecies)


# =============================================================================
# 3. Helper: build a spatial prey raster
# =============================================================================
# Start from a uniform base of 1 over sea cells. Bump specified cells by either
# a multiplier or an additive offset. The absolute scale doesn't matter because
# Par$Pmedian[simrun] rescales the whole map per replicate -- what matters is
# the *relative* pattern across cells.

make_spatial_prey <- function(seamask,
                              hotspot_cells = integer(0),
                              multiplier = 2.0,
                              additive = 0) {
  r <- raster::calc(seamask, fun = function(x) { x[x == 0] <- 1; return(x) })
  if (length(hotspot_cells) > 0) {
    vals <- raster::values(r)
    vals[hotspot_cells] <- vals[hotspot_cells] * multiplier + additive
    raster::values(r) <- vals
  }
  r
}


# =============================================================================
# 4. Define hotspot cell sets (the spatial design)
# =============================================================================
# In this seamask: sea cells have value 0, land cells are NaN. So the correct
# sea-cell selector is `values(seamask) == 0`, NOT `> 0`.
#
# A few patterns for building hotspot_cells:
#
#   # cells inside an ORD footprint
#   ord_cells <- unlist(raster::cellFromPolygon(seamask, sf::as_Spatial(ORDpoly)))
#
#   # cells within X metres of a colony point
#   colony_pt  <- sf::st_sfc(sf::st_point(c(spadat1$fltxy.E, spadat1$fltxy.N)),
#                            crs = raster::crs(seamask))
#   colony_buf <- sf::st_buffer(colony_pt, dist = 20000)  # 20 km
#   near_cells <- unlist(raster::cellFromPolygon(seamask, sf::as_Spatial(colony_buf)))

sea_cells <- which(raster::values(seamask) == 0)

near_colony_cells <- integer(0)                 # TODO: fill in once you decide
far_cells         <- integer(0)                 # TODO: fill in once you decide
ord_cells         <- integer(0)                 # TODO: fill in once you decide
random_cells      <- sample(sea_cells, size = min(200, length(sea_cells)))


# -----------------------------------------------------------------------------
# 4b. Build the transect-outside-ORD PreyMap (uses helper from transect_helpers.R)
# -----------------------------------------------------------------------------
# Transect lines randomly placed in a 20-40 km ring outside the windfarm,
# random orientation, 1 km corridor width, prey doubled in transect cells.
# Same layout used across all replicates of this scenario (see seed below).

transect_res <- make_transect_prey(
  seamask       = seamask,
  ORDpoly       = ORDpoly,
  min_distance  = 20000,   # transects begin >= 20 km outside windfarm
  max_distance  = 40000,   # ... and <= 40 km out (keep within foraging range)
  n_transects   = 10,
  length_m      = 8000,    # 8 km transect lines
  width_m       = 1000,    # 1 km corridor width
  multiplier    = 2.0,
  Pmedian_value = Par$Pmedian[1],   # base Pmedian for kJ accounting
  energy_prey   = spdat$energy_prey,
  seed          = 2026
)

# Save the transect layout for later reference (the same seed will reproduce it,
# but it's useful to have the geometries on disk).
dir.create("outputs", showWarnings = FALSE)
saveRDS(transect_res, "outputs/transect_scenario_geometry.rds")

# Visualise the resulting PreyMap with windfarm + transects overlaid.
plot_preymap(transect_res$PreyMap, ORDpoly,
             transects = transect_res$transects,
             ring      = transect_res$ring,
             file      = "outputs/preymap_transects.png",
             title     = sprintf("Transect prey enrichment (+%.0f kJ injected)",
                                 transect_res$energy_summary$total_extra_kJ))


# =============================================================================
# 5. Define scenarios
# =============================================================================
# Each scenario specifies:
#   - PreyType      : "Uniform" or "Map" (book-keeping; both scale by Pmedian)
#   - PreyMap       : NULL for uniform, or a RasterLayer for spatial
#   - Pmedian_scale : multiplier on the default Par$Pmedian, lets us do a sweep

scenarios <- list(

  # ----- Baseline + uniform sweep ------------------------------------------
  baseline      = list(PreyType = "Uniform", PreyMap = NULL, Pmedian_scale = 1.00),
  uniform_050   = list(PreyType = "Uniform", PreyMap = NULL, Pmedian_scale = 0.50),
  uniform_075   = list(PreyType = "Uniform", PreyMap = NULL, Pmedian_scale = 0.75),
  uniform_125   = list(PreyType = "Uniform", PreyMap = NULL, Pmedian_scale = 1.25),
  uniform_150   = list(PreyType = "Uniform", PreyMap = NULL, Pmedian_scale = 1.50),

  # ----- Spatially explicit scenarios --------------------------------------
  # Each spatial scenario doubles prey in its hotspot cells, with the same
  # default Pmedian as baseline so the comparison is "extra prey in hotspots".
  spatial_near_colony = list(
    PreyType = "Map",
    PreyMap  = make_spatial_prey(seamask, near_colony_cells, multiplier = 2),
    Pmedian_scale = 1.00
  ),
  spatial_far = list(
    PreyType = "Map",
    PreyMap  = make_spatial_prey(seamask, far_cells, multiplier = 2),
    Pmedian_scale = 1.00
  ),
  spatial_ord_overlap = list(
    PreyType = "Map",
    PreyMap  = make_spatial_prey(seamask, ord_cells, multiplier = 2),
    Pmedian_scale = 1.00
  ),
  spatial_random_hotspots = list(
    PreyType = "Map",
    PreyMap  = make_spatial_prey(seamask, random_cells, multiplier = 2),
    Pmedian_scale = 1.00
  ),

  # Random transect lines in a ring outside the windfarm (see section 4b).
  # PreyMap was built above; injects ~`total_extra_kJ` extra kJ into the system.
  spatial_transects_outside_ord = list(
    PreyType = "Map",
    PreyMap  = transect_res$PreyMap,
    Pmedian_scale = 1.00
  )
)


# =============================================================================
# 6. Run the scenarios
# =============================================================================
# Same initialseed across scenarios -> matched stochastic draws -> any
# difference in outputs is attributable to the prey distribution.

EXPERIMENT_SEED <- 12345

results <- purrr::imap(scenarios, function(sc, name) {
  message("--- Running scenario: ", name, " ---")

  Par_i <- Par
  Par_i$initialseed <- EXPERIMENT_SEED
  Par_i$PreyType    <- sc$PreyType
  Par_i$Pmedian     <- Par$Pmedian * sc$Pmedian_scale

  seabord(
    Par                  = Par_i,
    modPar               = modPar,
    ordPar               = ordPar,
    switches             = switches,
    seamask              = seamask,
    spadat1              = spadat1,
    spadat2              = spadat2,
    spdat                = spdat,
    BrdData              = BrdData,
    FrgCompData          = FrgCompData,
    fltdist_base         = fltdist_base,
    FlightGridcorrection = FlightGridcorrection,
    ORDpoly              = ORDpoly,
    PreyMap              = sc$PreyMap
  )
})


# =============================================================================
# 7. Combine outputs for comparison
# =============================================================================
# Each result has output_a0 (adults), output_c0 (chicks), output_y0 (yearly).
# These are lists of tibbles -- one per (simrun, season, tstep). We bind them
# together and tag with the scenario name.

bind_output <- function(slot) {
  purrr::imap_dfr(results, function(res, name) {
    dplyr::bind_rows(res[[slot]], .id = "rep_idx") %>%
      dplyr::mutate(scenario = name, .before = 1)
  })
}

adult_summary  <- bind_output("output_a0")
chick_summary  <- bind_output("output_c0")
yearly_summary <- bind_output("output_y0")


# =============================================================================
# 8. Save outputs
# =============================================================================

dir.create("outputs", showWarnings = FALSE)
saveRDS(results,        "outputs/prey_experiment_raw.rds")
saveRDS(adult_summary,  "outputs/prey_experiment_adults.rds")
saveRDS(chick_summary,  "outputs/prey_experiment_chicks.rds")
saveRDS(yearly_summary, "outputs/prey_experiment_yearly.rds")


# =============================================================================
# 9. Quick comparison (placeholder -- adapt to actual output columns)
# =============================================================================
# TODO: confirm the column names in output_a0/c0/y0 once you've run once.
# Typical metrics to summarise per scenario:
#   - adult survival rate (proportion is_alive == TRUE at end of season)
#   - mean adult body mass at end of season
#   - chick survival rate
#   - productivity (chicks fledged per pair)

# Example: adult survival rate by scenario, averaged across replicates.
# adult_summary %>%
#   dplyr::group_by(scenario, simrun) %>%
#   dplyr::summarise(surv = mean(is_alive, na.rm = TRUE), .groups = "drop") %>%
#   dplyr::group_by(scenario) %>%
#   dplyr::summarise(mean_surv = mean(surv), sd_surv = sd(surv))
