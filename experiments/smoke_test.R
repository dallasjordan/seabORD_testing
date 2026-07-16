################################################################################
## Smoke test (FAST version): confirm seabord() runs with the new PreyMap arg.
##
## Runs two minimal scenarios on the example data, with a 2-day season for speed:
##   1. baseline           -- PreyMap = NULL (existing uniform behaviour)
##   2. spatial_transects  -- PreyMap built from 2 random sea-only transects
##                            outside the windfarm
##
## On success, prints a short summary of each result.
################################################################################
rm(list=ls())
setwd("C:\\Users\\dallas.jordan\\OneDrive - SLR Consulting\\Projects\\seabORD_testing\\")
suppressPackageStartupMessages({
  library(seabORD)
  library(raster)
  library(sf)
  library(dplyr)
})

source("experiments/transect_helpers.R")

cat("seabORD version:", as.character(packageVersion("seabORD")), "\n")

# ----------------------------------------------------------------------------
# 1. Load example data
# ----------------------------------------------------------------------------
data("example_1_lists",            package = "seabORD")
data("seamask_3035_example",       package = "seabORD")
data("BrdData_example",            package = "seabORD")
data("frgcompdata_example",        package = "seabORD")
data("ORDpoly_example",            package = "seabORD")
data("UK9004171_bysea_3035",       package = "seabORD")
data("FlightGridcorrection_3035",  package = "seabORD")
data("energeticsandpreydata",      package = "seabORD")
data("spacoordinates",             package = "seabORD")
data("spalist",                    package = "seabORD")

# ----------------------------------------------------------------------------
# 2. Reconstruct rasters and tibble inputs per the vignette recipe
# ----------------------------------------------------------------------------
rebuild_raster <- function(rlist, name = NULL) {
  md <- rlist$metadata
  r <- raster::raster(
    nrows = md[["n_rows"]], ncols = md[["n_cols"]],
    xmn = md[["x_min"]],    xmx = md[["x_max"]],
    ymn = md[["y_min"]],    ymx = md[["y_max"]],
    crs = md[["crs"]]
  )
  r <- raster::setValues(r, rlist$matrix)
  if (!is.null(name)) names(r) <- name
  r
}

SPA_CODE <- "UK9004171"

seamask      <- rebuild_raster(seamask_3035_example, "seamask_3035")
BrdData      <- rebuild_raster(BrdData_example,      "Forth.Islands")
FrgCompData  <- rebuild_raster(frgcompdata_example,  "Forth.Islands")
fltdist_base <- raster::brick(rebuild_raster(UK9004171_bysea_3035, SPA_CODE))

spadat1 <- tibble::as_tibble(dplyr::filter(spacoordinates, SITECODE  == SPA_CODE))
spadat2 <- tibble::as_tibble(dplyr::filter(spalist,         SITE_CODE == SPA_CODE))

cat("Inputs loaded. seamask cells:", ncell(seamask),
    "| sea cells (value 0):", sum(values(seamask) == 0, na.rm = TRUE), "\n")
cat("spadat1 rows:", nrow(spadat1), "| spadat2 rows:", nrow(spadat2), "\n")

# ----------------------------------------------------------------------------
# 3. Parameter setup -- SHORTENED for smoke test
# ----------------------------------------------------------------------------
Par      <- example_1_lists$Par
modPar   <- example_1_lists$modPar
ordPar   <- example_1_lists$ordPar
switches <- example_1_lists$switches
ORDpoly  <- ORDpoly_example
FlightGridcorrection <- FlightGridcorrection_3035

spdat <- dplyr::filter(energeticsandpreydata, Code == Par$thisSpecies)

# >>> THE SPEED KNOB <<<  Shrink the season from 30 days to 2 days.
spdat$seasonlength <- 2L
cat("seasonlength forced to:", spdat$seasonlength, "(default would be 30)\n")

cat("thisSpecies:", Par$thisSpecies, "| spdat rows:", nrow(spdat), "\n")
cat("Nreplicates (modPar):", modPar$Nreplicates, "\n")

# ----------------------------------------------------------------------------
# 4. Build the spatial PreyMap from 2 transects placed entirely over sea
# ----------------------------------------------------------------------------
transect_res <- make_transect_prey(
  seamask       = seamask,
  ORDpoly       = ORDpoly_example,
  min_distance  = 20000,
  max_distance  = 40000,
  n_transects   = 2,        # smoke-test size
  length_m      = 8000,
  width_m       = 1000,
  multiplier    = 2.0,
  sea_only      = TRUE,     # reject any transect that touches a land cell
  Pmedian_value = Par$Pmedian[1],
  energy_prey   = spdat$energy_prey,
  seed          = 42
)
PreyMap_spatial <- transect_res$PreyMap

# Visualise the resulting PreyMap with windfarm + transects overlaid.
plot_preymap(transect_res$PreyMap, ORDpoly,
             transects = transect_res$transects,
             ring      = transect_res$ring,
             file      = "outputs/preymap_transects.png",
             title     = sprintf("Transect prey enrichment (+%.0f kJ injected)",
                                 transect_res$energy_summary$total_extra_kJ))

cat("Spatial PreyMap built: transects =", length(transect_res$transects),
    "| target cells =", length(transect_res$target_cells), "\n")
if (!is.null(transect_res$energy_summary)) {
  cat(sprintf("Energy injected: +%.0f g of prey  (=  +%.0f kJ)\n\n",
              transect_res$energy_summary$total_extra_mass_g,
              transect_res$energy_summary$total_extra_kJ))
}

# ----------------------------------------------------------------------------
# 5. Helper: run one scenario
# ----------------------------------------------------------------------------
run_scenario <- function(label, PreyType, PreyMap) {
  cat("=== Scenario:", label, " (PreyType =", PreyType, ", PreyMap =",
      if (is.null(PreyMap)) "NULL" else "raster", ") ===\n")

  Par_i <- Par
  Par_i$initialseed <- 12345
  Par_i$PreyType    <- PreyType

  t0 <- Sys.time()
  res <- tryCatch(
    seabord(
      Par = Par_i, modPar = modPar, ordPar = ordPar, switches = switches,
      seamask = seamask, spadat1 = spadat1, spadat2 = spadat2,
      spdat = spdat, BrdData = BrdData, FrgCompData = FrgCompData,
      fltdist_base = fltdist_base,
      FlightGridcorrection = FlightGridcorrection,
      ORDpoly = ORDpoly,
      PreyMap = PreyMap
    ),
    error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL }
  )
  t1 <- Sys.time()
  cat("Wall time:", round(as.numeric(t1 - t0, units = "secs"), 1), "s\n")

  if (!is.null(res)) {
    cat("Result names:", paste(names(res), collapse = ", "), "\n")
    if (!is.null(res$output_a0)) cat("output_a0 length:", length(res$output_a0), "\n")
  }
  cat("\n")
  invisible(res)
}

# ----------------------------------------------------------------------------
# 6. Run both scenarios
# ----------------------------------------------------------------------------
res_baseline <- run_scenario("baseline",          PreyType = "Uniform", PreyMap = NULL)
res_spatial  <- run_scenario("spatial_transects", PreyType = "Map",     PreyMap = PreyMap_spatial)

# ----------------------------------------------------------------------------
# 7. Outputs
# ----------------------------------------------------------------------------
# Baseline
adults_all   <- dplyr::bind_rows(res_baseline$output_a0)
chicks_all   <- dplyr::bind_rows(res_baseline$output_c0)
yearly_all   <- dplyr::bind_rows(res_baseline$output_y0)

dplyr::glimpse(adults_all)
dplyr::glimpse(yearly_all)

# Adult survival — proportion alive at end of season
end_of_season <- res_baseline$output_a0[[length(res_baseline$output_a0)]]
mean(end_of_season$is_alive, na.rm = TRUE)

# Or across all recorded steps:
adults_all %>%
  dplyr::group_by(simrun, season, tstep) %>%
  dplyr::summarise(survival = mean(is_alive, na.rm = TRUE), .groups = "drop")

# Adult body-mass distribution at end of season
summary(end_of_season$BM_adult)
hist(end_of_season$BM_adult, main = "End-of-season adult body mass")

# Chick survival / fledging
end_chicks <- res_baseline$output_c0[[length(res_baseline$output_c0)]]
mean(end_chicks$is_alive, na.rm = TRUE)

# Yearly summary already aggregates per simrun/season
yearly_all

# Baseline vs. Spatial
compare %>%
  dplyr::filter(Season == "scen") %>%
  dplyr::select(scenario, Rep, BM_adult.mn, BM_condition.mn, forage_g.mn)

compare %>%
  dplyr::group_by(scenario) %>%
  dplyr::summarise(
    forage_drop = forage_g.mn[Season == "scen"] - forage_g.mn[Season == "base"],
    bm_drop     = BM_adult.mn[Season == "scen"] - BM_adult.mn[Season == "base"]
  )

# Visualise flight paths
# Compute the zoom extent from windfarm + transect geometries, with some padding
bb <- sf::st_bbox(c(sf::st_geometry(ORDpoly),
                    sf::st_geometry(transect_res$transects)))
pad <- max(diff(bb[c("xmin","xmax")]), diff(bb[c("ymin","ymax")])) * 0.2
zoom_ext <- raster::extent(bb["xmin"] - pad, bb["xmax"] + pad,
                           bb["ymin"] - pad, bb["ymax"] + pad)

# Crop and plot
fm_zoom <- raster::crop(res_baseline$BirdFlightMap, zoom_ext)
raster::plot(fm_zoom, main = "Bird flight density (zoomed)")
plot(sf::st_geometry(ORDpoly),             add = TRUE, border = "black",     lwd = 2)
plot(sf::st_geometry(transect_res$transects), add = TRUE, col = "darkgreen", lwd = 2)

cat("=== SMOKE TEST DONE ===\n")
cat("baseline OK:", !is.null(res_baseline), "\n")
cat("spatial  OK:", !is.null(res_spatial),  "\n")
