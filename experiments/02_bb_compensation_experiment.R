################################################################################
## Experiment 02: how much offal offsets Berwick Bank?
##
##   without_BB : NnG + IC + SG        -> the target
##   with_BB    : NnG + IC + SG + BB   -> the loss
##   offal cell : add a dump site to the with_BB world and sweep the daily
##                deposit until productivity returns to the target
##
## Two alternative delivery mechanisms (spatial, per-bird) are retained but off
## by default; the offal cell is the reportable scenario.
##
## Study-wide inputs (colony size, population fraction, prey, displacement) come
## from _setup_inputs.R. Edit them there.
##
## RUNTIME: every config is a full-season run with N_REPLICATES reps and both
## seasons, ~4-5 h at 10 reps and 10% of a 6068-pair colony. Results and raw
## output are saved after each config and the script resumes from them, so an
## interruption costs at most one config. Prefer a background run.
################################################################################

source("experiments/_setup_inputs.R")

# =============================================================================
# Config
# =============================================================================
N_REPLICATES_BASELINE <- 20
N_REPLICATES_SWEEP    <- 20
N_REPLICATES <- N_REPLICATES_BASELINE

# Output files are keyed to the colony size so runs at different populations can
# never overwrite each other or resume from incomparable results.
RUN_TAG <- paste0("2c_", COLONY_PAIRS)

# Per-bird destinations: needed to confirm the offal forcing fired and to see
# where birds fed.
switches$saveperbirddest <- TRUE

# --- Which stages to run ------------------------------------------------------
RUN_BASELINES  <- TRUE    # without_BB and with_BB
RUN_SPATIAL    <- FALSE   # 2a: offal spread near the colony, found at random
RUN_PERBIRD    <- FALSE   # 2b: offal wherever birds already forage
RUN_OFFAL_CELL <- TRUE    # 2c: dump site birds fly to  <- main scenario

# Re-measure configs already present in this run's output files. Set via get0()
# so `FORCE_RERUN <- TRUE` in the console survives sourcing.
FORCE_RERUN <- get0("FORCE_RERUN", ifnotfound = FALSE)

# --- Windfarm configurations --------------------------------------------------
WF_WITHOUT_BB <- c(INCAP = TRUE, SEAGREEN = TRUE, NEART = TRUE, BERWICK = FALSE)
WF_WITH_BB    <- c(INCAP = TRUE, SEAGREEN = TRUE, NEART = TRUE, BERWICK = TRUE)

# --- Offal properties ---------------------------------------------------------
OFFAL_KJ_PER_G <- 9      # energy density available to kittiwakes (kJ/g)
# Applied to the WHOLE offal cell via EnergyMap, including the ~175 g of ordinary
# prey already there: birds at the dump site are assumed to feed preferentially
# on offal. At any realistic deposit offal dominates the cell anyway.

OFFAL_OTHER_SPECIES_LOSS <- 0.20   # share taken by gulls and other scavengers
KITTIWAKE_DAILY_G        <- 280    # one kittiwake-day of offal (g)

# =============================================================================
# 2a. Spatial offal near the colony (birds find it at random)
# =============================================================================
COLONY_RADIUS_M  <- 20000
SPATIAL_OFFAL_KG <- c(0, 2000, 5000, 10000, 20000)   # total biomass dumped (kg)

# =============================================================================
# 2b. Offal wherever birds already forage (isolates the food effect)
# =============================================================================
ACCESS_FRAC_FIXED <- 0.47                     # assumed, not derived
PERBIRD_OFFAL_KG  <- c(0, 0.2, 0.5, 1, 2, 5)  # kg per accessing bird per trip

# =============================================================================
# 2c. Offal cell near the colony that birds FLY TO  <- main scenario
# =============================================================================
OFFAL_CELL_RADIUS_M <- 20000   # search radius when auto-picking the dump site
OFFAL_CELL_KG <- c(0, 50, 100, 150, 200, 300, 500, 1000, 2000, 4000)  # kg per DAY

# --- Where the dump site goes -------------------------------------------------
# NULL  -> auto-pick the most-visited reachable sea cell within
#          OFFAL_CELL_RADIUS_M. That lands 1 km SOUTH of the colony (cell
#          1801817), the nearest sea cell there is, since the grid is 1 km.
# c(E,N) -> metres from the colony, snapped to the containing cell.
#
# The Isle of May occupies the colony cell AND the cell 1 km due east, so due
# east is only available from 2 km out. Nearby sea alternatives:
#   c(2000,  0)     2 km E
#   c(1000, -1000)  1 km E, 1 km S  (diagonal, ~1.4 km)
#   c(1000,  1000)  1 km E, 1 km N  (diagonal, ~1.4 km)
# Moving the site further out lengthens the commute for diverted birds, which
# reduces the travel saving bundled into the result -- the point of moving it.
OFFAL_CELL_OFFSET_M <- NULL

# --- How many birds a deposit can feed ----------------------------------------
# The access fraction is DERIVED from the deposit, not assumed:
#
#   meals  = deposit_g * (1 - OFFAL_OTHER_SPECIES_LOSS) / KITTIWAKE_DAILY_G
#   access = meals / adults in the REAL colony            [capped at 1.0]
#
# The fraction is scale-invariant, so the real colony is the right denominator
# even though only POP_FRACTION of it is simulated.
#
# WHY IT IS DONE THIS WAY: a cell's value is a STANDING STOCK, not a budget.
# seabORD offers it in full to every bird and resets it every timestep -- prey
# depletes within one bird's bout, never between birds or days. A raw daily drop
# is therefore never shared out, and nothing caps the colony's total intake at
# what was dropped. Allocating each admitted bird one KITTIWAKE_DAILY_G meal
# restores the accounting: the deposit constrains HOW MANY birds are fed rather
# than how richly. Measured intake (~180 g) sits below the 280 g allocation, so
# the birds collectively consume less than was dropped -- conservative.
offal_access_frac <- function(kg, n_adults_real) {
  if (kg <= 0) return(0)
  meals <- kg * 1000 * (1 - OFFAL_OTHER_SPECIES_LOSS) / KITTIWAKE_DAILY_G
  min(1, meals / n_adults_real)
}

# What each dose resolves to at COLONY_PAIRS = 6068 (12136 adults, 1214 simulated):
#
#     kg/day    meals   access   birds fed (of 1214)   standing g/bird
#         50      143    0.012                    14               286
#        100      286    0.024                    29               276
#        150      429    0.035                    43               279
#        200      571    0.047                    57               281
#        300      857    0.071                    86               279
#        500     1429    0.118                   143               280
#       1000     2857    0.235                   286               280
#       2000     5714    0.471                   572               280
#       4000    11429    0.942                  1143               280
#
# Two properties to understand before reading any result:
#  - Standing stock is ~280 g per fed bird at every dose BY CONSTRUCTION. The
#    dose does not change how well a fed bird eats; it changes HOW MANY are fed.
#  - All 12136 adults would need ~4250 kg/day, so nothing here saturates.
#
# The intervention is BUNDLED. A diverted bird also gets a shorter commute,
# immunity from displacement (it no longer routes past the windfarms) and a prey
# quality upgrade (6.52 -> 9 kJ/g). All arrive in full for any bird admitted to
# the patch, at any deposit. Report it as a dump-site intervention, not as offal
# energy alone.

colony_point <- COLONY_POINT

# =============================================================================
# Helpers
# =============================================================================
# Scen-season demographics per replicate, then mean + Monte Carlo SE.
#
# SURVIVAL: output_y0$AdultsSurvivingYr is referenced to the SAME CONFIG's own
# base season, so it measures the ORD effect WITHIN a config. The offal cell is
# present in both seasons and cancels out. It is kept as survival_native for
# diagnostics only, excluded from the threshold and the plot; the reportable
# figure comes from 03_comparable_survival.R. output_a0$N_alive_ad is no use
# either -- within-season adult survival is 1.0 by construction.
get_metrics <- function(res) {
  y <- dplyr::bind_rows(res$output_y0) %>%
    dplyr::filter(Season == "scen", !is.na(AdultsSurvivingYr))
  a <- dplyr::bind_rows(res$output_a0) %>%
    dplyr::filter(Season == "scen", !is.na(t)) %>%
    dplyr::group_by(Rep) %>% dplyr::filter(t == max(t)) %>% dplyr::ungroup() %>%
    dplyr::mutate(ml = (BM_adult_t0.mn - BM_adult.mn) / BM_adult_t0.mn)
  c0 <- dplyr::bind_rows(res$output_c0) %>%
    dplyr::filter(Season == "scen", !is.na(t)) %>%
    dplyr::group_by(Rep) %>% dplyr::filter(t == max(t)) %>% dplyr::ungroup()

  per_rep <- function(d, col) {
    d %>% dplyr::group_by(Rep) %>%
      dplyr::summarise(v = mean(.data[[col]], na.rm = TRUE), .groups = "drop") %>%
      dplyr::pull(v)
  }
  se <- function(x) { x <- x[is.finite(x)]
                      if (length(x) < 2) NA_real_ else stats::sd(x) / sqrt(length(x)) }

  s_rep <- per_rep(y,  "AdultsSurvivingYr")
  p_rep <- per_rep(c0, "ChicksPerNest")
  m_rep <- per_rep(a,  "ml")

  tibble::tibble(productivity       = mean(p_rep, na.rm = TRUE),
                 productivity_se    = se(p_rep),
                 mass_loss          = mean(m_rep, na.rm = TRUE),
                 mass_loss_se       = se(m_rep),
                 survival_native    = mean(s_rep, na.rm = TRUE),  # diagnostic only
                 survival_native_se = se(s_rep),
                 n_reps             = length(p_rep))
}

# Run one configuration and return its metrics plus labelling.
run_config <- function(windfarms, label, mechanism, offal_amount,
                       PreyMap = NULL, EnergyMap = NULL,
                       offal_frac = 0, offal_biomass_g = 0, offal_cell = NULL,
                       standing_g = NA_real_, access_frac = NA_real_,
                       n_access = NA_integer_) {
  message(sprintf("--- %s (%s = %s) ---", label, mechanism, offal_amount))
  wf <- load_windfarms(WINDFARM_SHP, target_crs = raster::crs(seamask),
                       include = names(windfarms)[windfarms])
  Par_i <- Par
  Par_i$Pmedian            <- rep(CALIBRATED_PMEDIAN, N_REPLICATES)
  Par_i$PreyType           <- if (is.null(PreyMap)) "Uniform" else "Map"
  Par_i$OffalAccessFrac    <- offal_frac       # 0 = no birds flagged for offal
  Par_i$OffalBiomass_g     <- offal_biomass_g  # >0 = offal wherever they forage (2b)
  Par_i$OffalCell          <- offal_cell       # non-NULL = fly to this cell (2c)
  Par_i$OffalEnergyDensity <- OFFAL_KJ_PER_G
  modPar_i <- modPar; modPar_i$Nreplicates <- N_REPLICATES
  ordPar_i <- ordPar; ordPar_i$include_ORDs <- wf$include_ORDs

  t0 <- Sys.time()
  res <- seabord(
    Par = Par_i, modPar = modPar_i, ordPar = ordPar_i, switches = switches,
    seamask = seamask, spadat1 = spadat1, spadat2 = spadat2, spdat = spdat,
    BrdData = BrdData, FrgCompData = FrgCompData, fltdist_base = fltdist_base,
    FlightGridcorrection = FlightGridcorrection, ORDpoly = wf$ORDpoly,
    PreyMap = PreyMap, EnergyMap = EnergyMap
  )
  m <- get_metrics(res)
  m$label <- label; m$mechanism <- mechanism; m$offal_amount <- offal_amount
  m$standing_g <- standing_g; m$access_frac <- access_frac; m$n_access <- n_access
  m$pmedian <- CALIBRATED_PMEDIAN   # recorded so resume can detect a changed calibration
  m$minutes <- round(as.numeric(Sys.time() - t0, units = "mins"), 1)
  message(sprintf("   productivity=%.4f (SE %.4f, n=%d)  mass loss=%.3f  (%.1f min)",
                  m$productivity, m$productivity_se, m$n_reps, m$mass_loss, m$minutes))

  # Keep everything seabord returned except BirdFlightMap (a 3.5M-cell raster per
  # config, ~28 MB, that we never analyse), so any metric can be recomputed later
  # without re-simulating.
  raw_store[[label]] <<- res[setdiff(names(res), "BirdFlightMap")]
  m
}

results   <- list()
raw_store <- list()

RES_RDS <- sprintf("outputs/bb_compensation_%s_results.rds", RUN_TAG)
RAW_RDS <- sprintf("outputs/bb_compensation_%s_raw.rds",     RUN_TAG)

save_progress <- function() {
  saveRDS(dplyr::bind_rows(results), RES_RDS)
  saveRDS(raw_store,                 RAW_RDS)
}

# --- Resume -------------------------------------------------------------------
# Everything in these files came from this colony size, so anything present is
# valid and is skipped.
if (file.exists(RES_RDS) && file.exists(RAW_RDS)) {
  prior <- readRDS(RES_RDS)
  # The output files are keyed to COLONY_PAIRS but not to the prey calibration,
  # so a Pmedian change between runs would otherwise resume silently and leave
  # the sweep half at one prey level and half at another. Refuse instead.
  if ("pmedian" %in% names(prior)) {
    old_pm <- unique(prior$pmedian[!is.na(prior$pmedian)])
    if (length(old_pm) && !all(old_pm == CALIBRATED_PMEDIAN)) {
      stop(sprintf(paste0("Stored configs were run at Pmedian %s but CALIBRATED_PMEDIAN ",
                          "is now %s. Resuming would mix prey levels. Move or delete\n  %s\n  %s\n",
                          "to start a clean sweep."),
                   paste(old_pm, collapse = "/"), CALIBRATED_PMEDIAN, RES_RDS, RAW_RDS))
    }
  }
  raw_store <- readRDS(RAW_RDS)
  for (i in seq_len(nrow(prior))) results[[prior$label[i]]] <- prior[i, ]
  message(sprintf("Resumed %d config(s) at Pmedian %d: %s",
                  length(results), CALIBRATED_PMEDIAN,
                  paste(names(results), collapse = ", ")))
}

# =============================================================================
# STAGE 1. Baselines
# =============================================================================
if (RUN_BASELINES) {
  N_REPLICATES <- N_REPLICATES_BASELINE
  for (lab in c("without_BB", "with_BB")) {
    if (!isTRUE(FORCE_RERUN) && !is.null(results[[lab]])) {
      message(sprintf("--- %s: already measured, skipping ---", lab)); next
    }
    wf <- if (lab == "without_BB") WF_WITHOUT_BB else WF_WITH_BB
    results[[lab]] <- run_config(wf, lab, "none", 0)
    save_progress()
  }
} else {
  stop("RUN_BASELINES is FALSE. Baselines must be measured at the same colony ",
       "size as the sweep (", COLONY_PAIRS, " pairs) -- there is no comparable ",
       "stored set to load. Set RUN_BASELINES <- TRUE.")
}

if (is.null(results[["without_BB"]]) || is.null(results[["with_BB"]])) {
  stop("Baselines missing -- cannot continue.")
}

target_prod <- results[["without_BB"]]$productivity
cat(sprintf("\nTARGET (without BB): productivity=%.4f (SE %.4f, n=%d)  mass loss=%.3f\n",
            target_prod, results[["without_BB"]]$productivity_se,
            results[["without_BB"]]$n_reps, results[["without_BB"]]$mass_loss))
cat(sprintf("WITH BB           : productivity=%.4f (SE %.4f, n=%d)  mass loss=%.3f\n",
            results[["with_BB"]]$productivity, results[["with_BB"]]$productivity_se,
            results[["with_BB"]]$n_reps, results[["with_BB"]]$mass_loss))
cat(sprintf("BB's cost         : productivity %+.4f (SE of the difference ~%.4f) = %+.0f chicks\n",
            results[["with_BB"]]$productivity - target_prod,
            sqrt(results[["without_BB"]]$productivity_se^2 +
                 results[["with_BB"]]$productivity_se^2),
            (results[["with_BB"]]$productivity - target_prod) * COLONY_PAIRS))
cat("Adult annual survival is NOT reported here -- run 03_comparable_survival.R.\n\n")

# =============================================================================
# STAGE 2a. Spatial offal near the colony
# =============================================================================
N_REPLICATES <- N_REPLICATES_SWEEP
if (RUN_SPATIAL) for (kg in SPATIAL_OFFAL_KG) {
  if (kg == 0) { pm <- NULL; em <- NULL } else {
    area <- make_area_prey(
      seamask = seamask, center = colony_point,
      Pmedian_value = CALIBRATED_PMEDIAN, energy_prey_model = spdat$energy_prey,
      min_distance = 0, max_distance = COLONY_RADIUS_M,
      reachable_only = TRUE, BrdData = BrdData,
      target_mass_g = kg * 1000, offal_energy_density = OFFAL_KJ_PER_G)
    pm <- area$PreyMap; em <- area$EnergyMap
  }
  key <- paste0("spatial_", kg, "kg")
  results[[key]] <- run_config(WF_WITH_BB, key, "spatial_kg", kg,
                               PreyMap = pm, EnergyMap = em)
  save_progress()
}

# =============================================================================
# STAGE 2b. Offal wherever the birds already forage
# =============================================================================
# ACCESS_FRAC_FIXED of adults feed on offal every trip but do NOT travel: they
# forage where BrdData sends them and find offal there. Isolates the food effect
# from flight cost and displacement exposure.
if (RUN_PERBIRD) {
  cat(sprintf("\n2b: %.0f%% of adults feed on offal where they already forage.\n",
              100 * ACCESS_FRAC_FIXED))
  for (kg in PERBIRD_OFFAL_KG) {
    key <- paste0("perbird_", kg, "kg")
    # kg = 0 -> switch access off so birds forage normally rather than being sent
    # to an empty offal source.
    fr <- if (kg == 0) 0 else ACCESS_FRAC_FIXED
    results[[key]] <- run_config(WF_WITH_BB, key, "perbird_offal_kg", kg,
                                 offal_frac = fr, offal_biomass_g = kg * 1000)
    save_progress()
  }
}

# =============================================================================
# STAGE 2c. Offal cell near the colony that birds FLY TO  <- main scenario
# =============================================================================
if (RUN_OFFAL_CELL) {
  # Resolve and validate the site up front. A bad location would otherwise fail
  # on the first non-zero dose, hours in.
  OFFAL_CELL_LOCATION <- if (is.null(OFFAL_CELL_OFFSET_M)) NULL else
    as.numeric(COLONY_XY + OFFAL_CELL_OFFSET_M)
  if (!is.null(OFFAL_CELL_LOCATION)) {
    .cell <- raster::cellFromXY(seamask, matrix(OFFAL_CELL_LOCATION, ncol = 2))
    .sv   <- if (is.na(.cell)) NA else raster::values(seamask)[.cell]
    if (is.na(.cell) || is.na(.sv) || .sv != 0)
      stop(sprintf(paste0("OFFAL_CELL_OFFSET_M = c(%s) is not a sea cell (the Isle of May ",
                          "occupies the colony cell and the one 1 km due east). Pick another ",
                          "offset -- c(2000, 0) or c(1000, -1000) are sea."),
                   paste(OFFAL_CELL_OFFSET_M, collapse = ", ")))
    .xy <- raster::xyFromCell(seamask, .cell)
    cat(sprintf("\n2c: dump site FIXED at cell %d, %+.0f m E %+.0f m N of the colony (%.2f km).\n",
                .cell, .xy[1] - COLONY_XY[1], .xy[2] - COLONY_XY[2],
                sqrt(sum((.xy - COLONY_XY)^2)) / 1000))
  } else {
    cat("\n2c: dump site auto-picked (most-visited reachable sea cell in radius).\n")
  }
  cat(sprintf("\n2c: sweeping the daily deposit; access fraction derived from it.\n"))
  cat(sprintf("    Colony %d adults (%d simulated at %.0f%%); %.0f%% of offal lost to other species;\n",
              N_ADULTS_REAL, N_ADULTS_SIM, 100 * POP_FRACTION,
              100 * OFFAL_OTHER_SPECIES_LOSS))
  cat(sprintf("    one kittiwake-day = %d g.\n", KITTIWAKE_DAILY_G))
  done <- names(results)
  for (kg in OFFAL_CELL_KG) {
    key <- paste0("offalcell_", kg, "kg")
    if (!isTRUE(FORCE_RERUN) && key %in% done) {
      message(sprintf("--- %s: already measured, skipping ---", key)); next
    }
    if (kg == 0) {
      # No deposit -> no patch, nobody sent anywhere. Birds forage exactly as in
      # with_BB. The no-intervention reference point.
      results[[key]] <- run_config(WF_WITH_BB, key, "offalcell_kg", kg,
                                   standing_g = 0)
    } else {
      frac     <- offal_access_frac(kg, N_ADULTS_REAL)
      n_access <- round(N_ADULTS_SIM * frac)
      if (n_access < 1) {
        message(sprintf("--- %s: feeds %.2f birds of %d -- below one bird, skipping ---",
                        key, N_ADULTS_SIM * frac, N_ADULTS_SIM))
        next
      }
      # The subsample's share of the deposit, split between the birds admitted to
      # the patch. Offal is added ON TOP of the cell's ordinary prey.
      standing_g <- kg * 1000 * (1 - OFFAL_OTHER_SPECIES_LOSS) * POP_FRACTION / n_access
      cat(sprintf("    %6.0f kg/day -> %5.1f%% access (%4d of %d birds), %6.0f g per bird (cell total %6.0f g)\n",
                  kg, 100 * frac, n_access, N_ADULTS_SIM, standing_g,
                  standing_g + CALIBRATED_PMEDIAN))
      # Site the cell: an explicit offset from the colony if one is set, else the
      # most-visited reachable sea cell within OFFAL_CELL_RADIUS_M.
      pt <- make_point_prey(
        seamask = seamask, center = COLONY_POINT, location = OFFAL_CELL_LOCATION,
        Pmedian_value = CALIBRATED_PMEDIAN, energy_prey_model = spdat$energy_prey,
        BrdData = BrdData, min_distance = 0, max_distance = OFFAL_CELL_RADIUS_M,
        target_mass_g = standing_g, offal_energy_density = OFFAL_KJ_PER_G)
      results[[key]] <- run_config(
        WF_WITH_BB, key, "offalcell_kg", kg,
        PreyMap = pt$PreyMap, EnergyMap = pt$EnergyMap,
        offal_frac = frac,            # derived from the deposit
        offal_biomass_g = 0,          # 0 -> use the CELL's prey, not a blanket override
        offal_cell = pt$target_cell,  # birds are sent here every trip
        standing_g = standing_g, access_frac = frac, n_access = n_access)
      # The site is identical across doses, so write it once. Keyed off the file
      # rather than the dose, since a resumed run may skip the first one.
      geom_file <- sprintf("outputs/offal_cell_geometry_%s.rds", RUN_TAG)
      if (!file.exists(geom_file)) saveRDS(pt, geom_file)
    }
    save_progress()
  }
}

# =============================================================================
# Analysis
# =============================================================================
res_df <- dplyr::bind_rows(results)

# Chicks, not proportions. ChicksPerNest is the share of nests fledging a chick
# and seabORD models one chick per nest, so proportion x pairs = chicks.
withbb_prod <- results[["with_BB"]]$productivity
res_df <- res_df %>%
  dplyr::mutate(
    colony_pairs        = COLONY_PAIRS,
    chicks_fledged      = productivity * COLONY_PAIRS,
    chicks_vs_with_BB   = (productivity - withbb_prod) * COLONY_PAIRS,
    chicks_vs_without_BB = (productivity - target_prod) * COLONY_PAIRS)

readr::write_csv(res_df, sprintf("outputs/bb_compensation_%s_results.csv", RUN_TAG))
cat("\n=== All configurations ===\n"); print(as.data.frame(res_df), row.names = FALSE)

cat("\n=== Chicks fledged per season (colony of ", COLONY_PAIRS, " pairs) ===\n", sep = "")
cat(sprintf("%-18s %8s %12s %14s\n", "config", "chicks", "vs with_BB", "vs without_BB"))
for (i in seq_len(nrow(res_df)))
  cat(sprintf("%-18s %8.0f %12.0f %14.0f\n", res_df$label[i], res_df$chicks_fledged[i],
              res_df$chicks_vs_with_BB[i], res_df$chicks_vs_without_BB[i]))

# The threshold analysis (how much offal offsets BB), the survival join and the
# report figure all live in 04_final_summary.R, which reads the files written
# above. Nothing below this point needs the simulation, so it is kept out of a
# 50-hour script.

cat(sprintf("\nDONE (%d pairs). Results: outputs/bb_compensation_%s_results.{rds,csv}\n",
            COLONY_PAIRS, RUN_TAG))
cat("Next: 03_comparable_survival.R, then 04_final_summary.R.\n")
