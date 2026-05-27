################################################################################
## Smoke test (FAST version): confirm seabord() runs with the new PreyMap arg.
##
## Runs two minimal scenarios on the example data, with a 2-day season for speed:
##   1. baseline       -- PreyMap = NULL (existing uniform behaviour)
##   2. spatial_random -- PreyMap = raster with 200 random sea cells at 2x
##
## On success, prints a short summary of each result.
################################################################################

suppressPackageStartupMessages({
  library(seabORD)
  library(raster)
  library(dplyr)
})

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
# 4. Build a simple spatial prey raster: uniform 1, with 200 random sea cells at 2.0
# ----------------------------------------------------------------------------
# Correct sea-cell selector: in this seamask sea = 0, land = NaN.
set.seed(42)
sea_cells <- which(values(seamask) == 0)
hotspot   <- sample(sea_cells, size = min(200, length(sea_cells)))

PreyMap_spatial <- calc(seamask, fun = function(x) { x[x == 0] <- 1; x })
vals <- values(PreyMap_spatial)
vals[hotspot] <- vals[hotspot] * 2.0
values(PreyMap_spatial) <- vals

cat("Spatial PreyMap built: hotspots in", length(hotspot), "cells (2x relative)\n\n")

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
res_baseline <- run_scenario("baseline",       PreyType = "Uniform", PreyMap = NULL)
res_spatial  <- run_scenario("spatial_random", PreyType = "Map",     PreyMap = PreyMap_spatial)

cat("=== SMOKE TEST DONE ===\n")
cat("baseline OK:", !is.null(res_baseline), "\n")
cat("spatial  OK:", !is.null(res_spatial),  "\n")
