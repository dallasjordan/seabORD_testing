################################################################################
## Experiment 02: single-cell prey enrichment outside the Forth/Tay windfarms
##
## Question: does adding a concentrated prey patch (a "feeding station") outside
## the windfarm complex change kittiwake productivity / survival, given the
## displacement caused by the windfarms?
##
## Design:
##   * Colony  : Forth Islands SPA (UK9004171), kittiwake (KI)
##   * Windfarms: Inch Cape, Seagreen, Neart na Gaoithe, Berwick Bank (all 4)
##   * Population: 5% subsample (Nscalefactor = 0.05) for a fast initial look
##   * Season  : full 30-timestep breeding season
##   * Pmedian : FIXED (so the injected biomass is deterministic across reps)
##   * Two prey scenarios, same seeds / windfarms / everything else:
##       - baseline : uniform prey (PreyMap = NULL)
##       - enriched : one sea cell outside the windfarms holds a large prey patch
##
##   Each seabord() run also internally produces a 'base' (no-ORD) and 'scen'
##   (with-ORD displacement) season, so you get the ORD effect for free.
##
## ---------------------------------------------------------------------------
## IMPORTANT REACHABILITY CAVEAT (validated 2026-07):
##   A SINGLE enriched 1 km cell is reached by only ~0.04% of total foraging
##   pressure (even the best-trafficked ring cell). Over a full run that is a
##   handful of bird-visits, so it will NOT move population-mean survival or
##   productivity. The prey MECHANISM works (verified: a broad 5x enrichment
##   cuts foraging_h from ~10.9 h to ~3.4 h and lifts body condition to ~1.0) --
##   the issue is purely that one cell is too small a target to find.
##
##   To get a DETECTABLE population signal, use make_area_prey() (broad zone,
##   see the commented block in section 4). The single-cell version below is
##   kept because it matches the original spec; run it to see the (near-null)
##   result and the diagnostics, then switch to the area design.
## ---------------------------------------------------------------------------
################################################################################

# --- Make sure the real user library (where seabORD is installed) is reachable,
#     even if this project's renv .Rprofile activated an empty renv library. ---
.libPaths(c("C:/Users/dallas.jordan/AppData/Local/R/win-library/4.6", .libPaths()))

suppressPackageStartupMessages({
  library(seabORD)
  library(raster)
  library(sf)
  library(dplyr)
  library(purrr)
  library(tibble)
})

setwd("C:/Users/dallas.jordan/OneDrive - SLR Consulting/Projects/seabORD_testing")
source("experiments/transect_helpers.R")
source("experiments/windfarms.R")

dir.create("outputs", showWarnings = FALSE)

# =============================================================================
# 1. Load inputs
# =============================================================================
data("example_1_lists",           package = "seabORD")
data("seamask_3035_example",      package = "seabORD")
data("BrdData_example",           package = "seabORD")
data("frgcompdata_example",       package = "seabORD")
data("UK9004171_bysea_3035",      package = "seabORD")
data("FlightGridcorrection_3035", package = "seabORD")
data("energeticsandpreydata",     package = "seabORD")
data("spacoordinates",            package = "seabORD")
data("spalist",                   package = "seabORD")

rebuild_raster <- function(rlist, name = NULL) {
  md <- rlist$metadata
  r <- raster::setValues(
    raster::raster(nrows = md[["n_rows"]], ncols = md[["n_cols"]],
                   xmn = md[["x_min"]], xmx = md[["x_max"]],
                   ymn = md[["y_min"]], ymx = md[["y_max"]], crs = md[["crs"]]),
    rlist$matrix)
  if (!is.null(name)) names(r) <- name
  r
}

SPA_CODE <- "UK9004171"

seamask      <- rebuild_raster(seamask_3035_example, "seamask_3035")
BrdData      <- rebuild_raster(BrdData_example,      "Forth.Islands")
FrgCompData  <- rebuild_raster(frgcompdata_example,  "Forth.Islands")
fltdist_base <- raster::brick(rebuild_raster(UK9004171_bysea_3035, SPA_CODE))
spadat1      <- tibble::as_tibble(dplyr::filter(spacoordinates, SITECODE  == SPA_CODE))
spadat2      <- tibble::as_tibble(dplyr::filter(spalist,         SITE_CODE == SPA_CODE))
spdat        <- dplyr::filter(energeticsandpreydata, Code == "KI")
FlightGridcorrection <- FlightGridcorrection_3035

# BrdData is stored keyed by "colony_species"; seabord looks it up by that name.
names(BrdData) <- paste(SPA_CODE, "KI", sep = "_")

# =============================================================================
# 2. Windfarms (all 4, incl. Berwick Bank) -> ORDpoly + matching include_ORDs
# =============================================================================
wf <- load_windfarms(
  "data/ShapefilesForSeabORD/WindfarmsForSeabORD.shp",
  target_crs = raster::crs(seamask),
  include    = c("INCAP", "SEAGREEN", "NEART", "BERWICK")   # BERWICK = not yet built
)
ORDpoly <- wf$ORDpoly

# =============================================================================
# 3. Parameters
# =============================================================================
Par      <- example_1_lists$Par
modPar   <- example_1_lists$modPar
ordPar   <- example_1_lists$ordPar
switches <- example_1_lists$switches

# --- Experiment knobs ---
POP_FRACTION  <- 0.05    # 5% of the Isle of May kittiwake population, for speed
N_REPLICATES  <- 3       # small for a quick look; bump to >= 20 for real inference
FIXED_PMEDIAN <- 158     # g/cell baseline prey (fixed so injection is deterministic)

# --- Offal knobs (edit these) ---
OFFAL_KG       <- 2000   # biomass of offal dumped in the enriched cell
OFFAL_KJ_PER_G <- 9      # energy density of offal AS AVAILABLE TO KITTIWAKES (kJ/g).
                         # Birds foraging in this cell extract 9 kJ per gram here,
                         # vs the species default (6.52 kJ/g) everywhere else.
                         # => cell energy availability = 2000 kg * 9 kJ/g = 18,000,000 kJ.

Par$Nscalefactor <- POP_FRACTION
Par$Pmedian      <- rep(FIXED_PMEDIAN, N_REPLICATES)   # length must equal Nreplicates
modPar$Nreplicates <- N_REPLICATES

# ORDs: the include_ORDs vector MUST match ORDpoly row-for-row (positional).
ordPar$include_ORDs <- wf$include_ORDs
cat("Windfarms in this run:", paste(ordPar$include_ORDs, collapse = ", "), "\n")
cat("Simulated pairs:", ceiling(Par$Npairspercol * POP_FRACTION),
    "of", Par$Npairspercol, "(", 100*POP_FRACTION, "% )\n")

# =============================================================================
# 4. Build the enriched-cell PreyMap + EnergyMap (offal)
# =============================================================================
# Dumps OFFAL_KG of biomass into one cell (PreyMap) and marks that cell as
# OFFAL_KJ_PER_G kJ/g (EnergyMap), so birds foraging there extract offal-quality
# energy. Cell energy availability = OFFAL_KG * OFFAL_KJ_PER_G.
#
# WHERE: by default the patch is auto-placed in the most-visited reachable
# (BrdData > 0) sea cell within a 20-40 km ring outside the windfarm complex.
# ***You specify the location*** by uncommenting `location = c(x, y)` (seamask
# EPSG:3035 coords) to pin it to an exact spot.

point_res <- make_point_prey(
  seamask           = seamask,
  ORDpoly           = ORDpoly,
  Pmedian_value     = FIXED_PMEDIAN,
  energy_prey_model = spdat$energy_prey,       # species default: 6.52 kJ/g
  BrdData           = BrdData,                 # used to auto-pick a reachable cell
  min_distance      = 20000,
  max_distance      = 40000,
  target_mass_g        = OFFAL_KG * 1000,      # biomass of offal dumped (grams)
  offal_energy_density = OFFAL_KJ_PER_G        # offal quality (kJ/g) in this cell
  # location = c(3600000, 3760000)             # <- uncomment to pin an exact cell
)
PreyMap_enriched   <- point_res$PreyMap
EnergyMap_enriched <- point_res$EnergyMap

print_injection(point_res$injection)

# --- ALTERNATIVE (recommended for a detectable effect): broad-area enrichment ---
# Enrich EVERY reachable sea cell in the 20-40 km ring outside the windfarms,
# spreading the same 2000 kg across them. Uncomment to use instead of the point.
#
# area_res <- make_area_prey(
#   seamask = seamask, ORDpoly = ORDpoly,
#   Pmedian_value = FIXED_PMEDIAN, energy_prey_model = spdat$energy_prey,
#   min_distance = 20000, max_distance = 40000, reachable_only = TRUE, BrdData = BrdData,
#   target_kJ = 18e6, target_energy_density = 9
# )
# PreyMap_enriched   <- area_res$PreyMap
# EnergyMap_enriched <- NULL   # area version does not set offal quality (biomass only)
# point_res <- area_res        # so downstream diagnostics/plots use the area cells
# print_injection(area_res$injection)

# =============================================================================
# 5. Diagnostics -- run BEFORE the (slow) simulation
# =============================================================================
cat("\n")
diag <- diagnose_preymap(PreyMap_enriched, BrdData, seamask, spdat, FIXED_PMEDIAN,
                         target_cells = point_res$target_cells)

# Visualise the enriched map with windfarms + patch location
plot_preymap(PreyMap_enriched, ORDpoly,
             features = point_res$point, ring = point_res$ring,
             file  = "outputs/preymap_point_enrichment.png",
             title = "Enriched prey cell (X) outside windfarms")

saveRDS(point_res, "outputs/point_enrichment_geometry.rds")

# =============================================================================
# 6. Run the two scenarios (same seed, same windfarms; only prey differs)
# =============================================================================
scenarios <- list(
  baseline = list(PreyMap = NULL,             EnergyMap = NULL),               # uniform
  enriched = list(PreyMap = PreyMap_enriched, EnergyMap = EnergyMap_enriched)  # offal cell
)

run_one <- function(sc, label) {
  message("=== Running scenario: ", label, " ===")
  Par_i <- Par
  Par_i$PreyType <- if (is.null(sc$PreyMap)) "Uniform" else "Map"
  t0 <- Sys.time()
  res <- seabord(
    Par = Par_i, modPar = modPar, ordPar = ordPar, switches = switches,
    seamask = seamask, spadat1 = spadat1, spadat2 = spadat2, spdat = spdat,
    BrdData = BrdData, FrgCompData = FrgCompData, fltdist_base = fltdist_base,
    FlightGridcorrection = FlightGridcorrection, ORDpoly = ORDpoly,
    PreyMap = sc$PreyMap, EnergyMap = sc$EnergyMap
  )
  message(sprintf("   done in %.1f min", as.numeric(Sys.time() - t0, units = "mins")))
  res
}

results <- purrr::imap(scenarios, ~ run_one(.x, .y))
saveRDS(results, "outputs/point_enrichment_results.rds")

# =============================================================================
# 7. Quick comparison of adult outcomes (base vs scen, baseline vs enriched)
# =============================================================================
adult <- purrr::imap_dfr(results, function(res, scen) {
  dplyr::bind_rows(res$output_a0) %>% dplyr::mutate(scenario = scen, .before = 1)
})

comparison <- adult %>%
  dplyr::select(scenario, Rep, Season, t, N_alive_ad, N_dead_ad,
                BM_adult.mn, BM_condition.mn, forage_g.mn) %>%
  dplyr::arrange(scenario, Rep, Season)

cat("\n=== Adult outcome comparison ===\n")
print(as.data.frame(comparison))

# The headline contrast: does enrichment lift foraging / condition in the
# ORD-displacement ('scen') season?
cat("\n=== Enrichment effect within the ORD ('scen') season ===\n")
adult %>%
  dplyr::filter(Season == "scen") %>%
  dplyr::group_by(scenario) %>%
  dplyr::summarise(mean_forage_g   = mean(forage_g.mn),
                   mean_BM_adult    = mean(BM_adult.mn),
                   mean_BM_condition = mean(BM_condition.mn),
                   .groups = "drop") %>%
  as.data.frame() %>% print()

cat("\nDONE. Raw results saved to outputs/point_enrichment_results.rds\n")
