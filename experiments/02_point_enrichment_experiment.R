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

# --- Shared setup: inputs, helpers, base parameter lists, calibrated Pmedian ---
source("experiments/_setup_inputs.R")

# =============================================================================
# 2. Windfarms -> ORDpoly + matching include_ORDs
# =============================================================================
# Toggle which windfarms are present. Set a name to FALSE to exclude it
# (e.g. BERWICK = FALSE to run without the not-yet-built Berwick Bank).
WINDFARMS <- c(INCAP = TRUE, SEAGREEN = TRUE, NEART = TRUE, BERWICK = TRUE)

wf <- load_windfarms(
  WINDFARM_SHP,
  target_crs = raster::crs(seamask),
  include    = names(WINDFARMS)[WINDFARMS]   # only the ones toggled TRUE
)
ORDpoly <- wf$ORDpoly
cat("Windfarms present:", paste(wf$include_ORDs, collapse = ", "), "\n")

# =============================================================================
# 3. Parameters
# =============================================================================
# Par / modPar / ordPar / switches come from _setup_inputs.R.

# Export per-bird foraging destinations (one row per bird per timestep).
switches$saveperbirddest <- TRUE

# --- Experiment knobs ---
POP_FRACTION  <- 0.05                # 5% of the Isle of May kittiwake population, for speed
N_REPLICATES  <- 3                   # small for a quick look; bump to >= 20 for real inference
FIXED_PMEDIAN <- CALIBRATED_PMEDIAN  # calibrated baseline prey (176; from experiment 03)

# --- Offal knobs (edit these) ---
OFFAL_KG       <- 2000   # biomass of offal dumped in the enriched cell
OFFAL_KJ_PER_G <- 9      # energy density of offal AS AVAILABLE TO KITTIWAKES (kJ/g).
                         # Birds foraging on offal extract 9 kJ per gram,
                         # vs the species default (6.52 kJ/g) on natural prey.
                         # => cell energy availability = 2000 kg * 9 kJ/g = 18,000,000 kJ.

# --- Per-bird offal access (decoupled from geography) ---
# A fixed fraction of birds forage on offal on EVERY trip, no matter where
# BrdData sends them. This is the mechanism to use to guarantee that e.g. 47% of
# the population accesses offal (unlike a spatial cell, which few birds reach).
# Controlled per-scenario in section 6; set the fraction here.
OFFAL_ACCESS_FRAC <- 0.47   # e.g. 0.47 = 47% of birds; used by the 'offal_access' scenario

Par$Nscalefactor <- POP_FRACTION
Par$Pmedian      <- rep(FIXED_PMEDIAN, N_REPLICATES)   # length must equal Nreplicates
modPar$Nreplicates <- N_REPLICATES

# Offal quality/quantity available to an offal-accessing bird each trip.
# (OffalAccessFrac is set per-scenario in run_one below.)
Par$OffalBiomass_g     <- OFFAL_KG * 1000     # grams available per trip (>> saturation = reliable)
Par$OffalEnergyDensity <- OFFAL_KJ_PER_G      # kJ/g the accessing birds extract

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
  # location = c(3600000, 3760000)             # <- uncomment to pin an exact cell;
                                               # x = 3542466 and y = 3740659 is a good one as it is most visited location
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
# Each scenario sets its own prey maps and offal-access fraction. Comment out
# any you don't want to run.
scenarios <- list(
  # Control: uniform prey, no offal at all
  baseline     = list(PreyMap = NULL,             EnergyMap = NULL,               OffalAccessFrac = 0),
  # Spatial: one offal cell outside the windfarms (reached by ~0.04% of trips)
  enriched     = list(PreyMap = PreyMap_enriched, EnergyMap = EnergyMap_enriched, OffalAccessFrac = 0),
  # Per-bird: a fixed % of birds forage on offal every trip (geography-independent)
  offal_access = list(PreyMap = NULL,             EnergyMap = NULL,               OffalAccessFrac = OFFAL_ACCESS_FRAC)
)

run_one <- function(sc, label) {
  message("=== Running scenario: ", label, " ===")
  Par_i <- Par
  Par_i$PreyType       <- if (is.null(sc$PreyMap)) "Uniform" else "Map"
  Par_i$OffalAccessFrac <- sc$OffalAccessFrac    # 0 disables the per-bird override
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

# =============================================================================
# 8. Per-bird foraging destinations
# =============================================================================
# res$output_dest: one row per bird per timestep, with the intended (FirstChoice)
# and actual (Destination) foraging cell numbers + their EPSG:3035 coordinates,
# whether the bird was displaced, and the flight distance (ActualKm). This is
# destination POINTS, not tracks -- seabORD has no movement paths.

dest <- purrr::imap_dfr(results, function(res, scen) {
  if (is.null(res$output_dest)) return(NULL)
  dplyr::mutate(res$output_dest, scenario = scen, .before = 1)
})

if (nrow(dest) > 0) {
  saveRDS(dest, "outputs/point_enrichment_destinations.rds")
  readr::write_csv(dest, "outputs/point_enrichment_destinations.csv")
  cat(sprintf("\nPer-bird destinations: %d rows (%d birds x timesteps x seasons x reps x scenarios)\n",
              nrow(dest), dplyr::n_distinct(dest$BirdID)))
  cat("Saved to outputs/point_enrichment_destinations.{rds,csv}\n")

  # How many bird-visits landed in the enriched cell?
  hits <- sum(dest$Destination == point_res$target_cell)
  cat(sprintf("Bird-visits to the enriched cell (%d): %d of %d (%.3f%%)\n",
              point_res$target_cell, hits, nrow(dest), 100 * hits / nrow(dest)))

  # Map the actual foraging destinations (enriched 'scen' season), windfarms + patch
  d1 <- dplyr::filter(dest, scenario == "enriched", Season == "scen")
  png("outputs/destinations_map.png", width = 1200, height = 1000, res = 150)
  bb <- sf::st_bbox(c(sf::st_geometry(ORDpoly), point_res$point))
  pad <- 30000
  plot(raster::crop(seamask, raster::extent(bb["xmin"]-pad, bb["xmax"]+pad,
                                            bb["ymin"]-pad, bb["ymax"]+pad)),
       col = "lightblue", legend = FALSE, main = "Foraging destinations (enriched, scen)")
  points(d1$dest_x, d1$dest_y, pch = 16, cex = 0.4,
         col = grDevices::adjustcolor("navy", 0.35))
  plot(sf::st_geometry(ORDpoly), add = TRUE, border = "black", lwd = 2)
  plot(point_res$point, add = TRUE, col = "red", pch = 4, cex = 2, lwd = 3)
  dev.off()
  cat("Destination map saved to outputs/destinations_map.png\n")
}

cat("\nDONE. Raw results saved to outputs/point_enrichment_results.rds\n")
