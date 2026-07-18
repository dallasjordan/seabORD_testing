################################################################################
## Experiment 04: how much offal offsets Berwick Bank?
##
## 1. WITHOUT_BB : NnG + IC + SG            -> survival & productivity  (target)
## 2. WITH_BB    : NnG + IC + SG + BB       -> how much they drop
## 3. Add offal near the Isle of May to the WITH_BB world and sweep the offal
##    amount until survival & productivity climb back to the WITHOUT_BB target.
##    Two delivery mechanisms are swept and compared:
##      3a. SPATIAL : offal in a disc of reachable cells around the colony
##                    (titrate total biomass, kg)
##      3b. PER-BIRD: a fraction of birds feed on offal every trip
##                    (titrate the access fraction, %)
##
## Output: the offal amount (each mechanism) that restores survival AND
## productivity to the WITHOUT_BB levels -> "how much offal offsets BB".
##
## Shared inputs come from _setup_inputs.R (edit inputs there, not here).
##
## RUNTIME WARNING: every row below is a full-season seabord run with
## N_REPLICATES reps and BOTH seasons. With the default sweeps that is ~12 runs;
## budget accordingly and prefer a background/overnight run. Results are saved
## after every run so a crash loses at most one row.
################################################################################

source("experiments/_setup_inputs.R")

# =============================================================================
# Config
# =============================================================================
POP_FRACTION <- 0.1      # 10% of the Isle of May population (~580 birds)

# Replication is split by STAGE, because the two questions need very different
# precision (at ~7 min per rep, 10% pop, full season):
#
#  Stage 1 - BB impact (the reportable result). Annual survival's BB effect is
#    only ~1-3 pp and AdultsSurvivingYr is a binomial draw (SE ~1.8 pp per rep
#    at 10% pop), so it needs ~20 reps to reach ~0.4 pp precision.
#    2 configs x 20 reps ~= 5 h.
#  Stage 2 - offal compensation. Driven by productivity, whose BB effect is
#    ~14 pp (far above the noise), so a few reps suffice.
#    11 configs x 3 reps ~= 5 h.
#
# Running line-by-line: set N_REPLICATES before each stage.
N_REPLICATES_BASELINE <- 20   # stage 1: without_BB / with_BB
N_REPLICATES_SWEEP    <- 10    # stage 2: the offal sweeps
N_REPLICATES <- N_REPLICATES_BASELINE

OFFAL_KJ_PER_G <- 9      # offal quality (kJ/g available to kittiwakes)

# --- Which analyses to run (toggle off the ones you don't need) ---------------
RUN_BASELINES  <- TRUE    # stage 1: without_BB vs with_BB  (the reportable BB impact)
RUN_OFFAL_CELL <- TRUE    # stage 2c: offal cell near colony, birds FLY to it  <- main scenario
RUN_SPATIAL    <- FALSE   # stage 2a: offal spread near colony, found randomly
RUN_PERBIRD    <- FALSE   # stage 2b: offal wherever birds forage (no travel effect)

# Displacement (NatureScot: 30%, 2 km buffer) is set study-wide in
# _setup_inputs.R so every experiment uses the same assumption.

# Windfarm configurations (positional toggles resolved by load_windfarms)
WF_WITHOUT_BB <- c(INCAP = TRUE, SEAGREEN = TRUE, NEART = TRUE, BERWICK = FALSE)
WF_WITH_BB    <- c(INCAP = TRUE, SEAGREEN = TRUE, NEART = TRUE, BERWICK = TRUE)

# --- 3a. SPATIAL offal near the colony; birds find it RANDOMLY (via BrdData) ---
COLONY_RADIUS_M  <- 20000                            # disc radius around the Isle of May
SPATIAL_OFFAL_KG <- c(0, 2000, 5000, 10000, 20000)   # total biomass dumped (kg)

# --- 3b. Offal cell that a GUARANTEED fraction of birds always feed on ---------
# ACCESS_FRAC is a fixed ASSUMPTION here (not swept): this share of adults feeds
# on the offal every trip. We sweep the offal AMOUNT to find how much is needed.
ACCESS_FRAC_FIXED <- 0.47                            # 47% of adults always feed on offal
PERBIRD_OFFAL_KG  <- c(0, 0.2, 0.5, 1, 2, 5)         # offal available per accessing bird per trip (kg)
# NB: the intake half-saturation (IR_half_a) is 900 g, so this sweep deliberately
# spans below and above saturation -- past ~2-3 kg birds simply max out their
# intake and extra offal stops helping.

# --- 2c. OFFAL CELL near the colony that birds FLY TO (the main scenario) ------
# We dump offal in one cell near the Isle of May and OFFAL_CELL_ACCESS_FRAC of
# adults forage there on every trip instead of their normal destination. Unlike
# 3b, they really travel to that cell, so this also captures:
#   - the shorter commute (flight is ~23% of daily energy at 4.7 h),
#   - the fact that they no longer route past Berwick Bank, so displacement
#     cannot touch them.
# That makes it the realistic "dump site" scenario -- but note the compensation
# it buys is offal energy PLUS avoided travel/displacement, not offal alone.
OFFAL_CELL_ACCESS_FRAC <- 0.47   # share of adults that fly to the offal cell
OFFAL_CELL_RADIUS_M    <- 20000  # search radius around the colony for the site
OFFAL_CELL_KG          <- c(0, 0.2, 0.5, 1, 2, 5)   # offal in the cell (kg), swept

colony_point <- COLONY_POINT   # from _setup_inputs.R

# =============================================================================
# Helpers
# =============================================================================
# Pull scen-season demographics, averaged over replicates.
#
# SURVIVAL: use output_y0$AdultsSurvivingYr -- the ANNUAL survival probability
# seabORD derives from end-of-season mass loss (via the species massloss_*/
# basesurv_* lookups). This is the real demographic pathway (~0.75).
# Do NOT use output_a0's N_alive_ad: no adult dies during the 30-day chick-rearing
# season at realistic prey, so within-season survival is 1.0 by construction and
# carries no signal (confirmed: zero adult deaths in the package's own scenario
# AND calibration example outputs).
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
  tibble::tibble(survival     = mean(y$AdultsSurvivingYr, na.rm = TRUE),  # annual
                 mass_loss    = mean(a$ml, na.rm = TRUE),                 # drives survival
                 productivity = mean(c0$ChicksPerNest, na.rm = TRUE))
}

# Run one full configuration and return its metrics + a label row.
run_config <- function(windfarms, label, mechanism, offal_amount,
                       PreyMap = NULL, EnergyMap = NULL,
                       offal_frac = 0, offal_biomass_g = 0, offal_cell = NULL) {
  message(sprintf("--- %s (%s = %s) ---", label, mechanism, offal_amount))
  wf <- load_windfarms(WINDFARM_SHP, target_crs = raster::crs(seamask),
                       include = names(windfarms)[windfarms])
  Par_i <- Par
  Par_i$Nscalefactor       <- POP_FRACTION
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
  m$minutes <- round(as.numeric(Sys.time() - t0, units = "mins"), 1)
  message(sprintf("   annual survival=%.4f  mass loss=%.3f  productivity=%.4f  (%.1f min)",
                  m$survival, m$mass_loss, m$productivity, m$minutes))

  # Keep the raw summary tibbles so ANY metric can be recomputed later without
  # re-simulating. (BirdFlightMap is dropped -- a 3.5M-cell raster per config
  # would bloat the file for no analytical gain.)
  raw_store[[label]] <<- list(output_a0 = res$output_a0,
                              output_c0 = res$output_c0,
                              output_y0 = res$output_y0)
  m
}

results   <- list()
raw_store <- list()
save_progress <- function() {
  saveRDS(dplyr::bind_rows(results), "outputs/bb_compensation_results.rds")
  saveRDS(raw_store,                  "outputs/bb_compensation_raw.rds")
}

# =============================================================================
# STAGE 1 (1 & 2). Baselines -- the reportable BB impact on survival AND
# productivity. Run at high replication to detect adult survival changes.
# =============================================================================
if (RUN_BASELINES) {
  N_REPLICATES <- N_REPLICATES_BASELINE
  results$without_bb <- run_config(WF_WITHOUT_BB, "without_BB", "none", 0); save_progress()
  results$with_bb    <- run_config(WF_WITH_BB,    "with_BB",    "none", 0); save_progress()
}

if (!is.null(results$without_bb)) {
  target_surv <- results$without_bb$survival
  target_prod <- results$without_bb$productivity
  cat(sprintf("\nTARGET (without BB): annual survival=%.4f  mass loss=%.3f  productivity=%.4f\n",
              target_surv, results$without_bb$mass_loss, target_prod))
  cat(sprintf("WITH BB           : annual survival=%.4f  mass loss=%.3f  productivity=%.4f\n",
              results$with_bb$survival, results$with_bb$mass_loss, results$with_bb$productivity))
  cat(sprintf("BB's cost         : survival %+.4f   productivity %+.4f   (the gap to close)\n\n",
              results$with_bb$survival - target_surv,
              results$with_bb$productivity - target_prod))
}

# =============================================================================
# STAGE 2 (3a). Spatial offal near the colony -- sweep biomass
# =============================================================================
N_REPLICATES <- N_REPLICATES_SWEEP   # productivity-driven; fewer reps suffice
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
# STAGE 2b. Offal wherever the birds already forage -- sweep the offal AMOUNT
# =============================================================================
# ACCESS_FRAC_FIXED of adults feed on offal every trip, but they do NOT travel:
# they forage where BrdData sends them and simply find offal there. This isolates
# the FOOD effect (no change to flight cost or displacement exposure).
if (RUN_PERBIRD) {
  cat(sprintf("\n2b: %.0f%% of adults feed on offal where they already forage; sweeping the amount.\n",
              100 * ACCESS_FRAC_FIXED))
  for (kg in PERBIRD_OFFAL_KG) {
    key <- paste0("perbird_", kg, "kg")
    # kg = 0 means no offal at all -> switch access off so birds forage normally
    # (rather than being sent to an empty offal source and starving).
    fr <- if (kg == 0) 0 else ACCESS_FRAC_FIXED
    results[[key]] <- run_config(WF_WITH_BB, key, "perbird_offal_kg", kg,
                                 offal_frac = fr, offal_biomass_g = kg * 1000)
    save_progress()
  }
}

# =============================================================================
# STAGE 2c. OFFAL CELL near the colony that birds FLY TO  <-- main scenario
# =============================================================================
# One cell near the Isle of May holds offal, and OFFAL_CELL_ACCESS_FRAC of adults
# forage there every trip instead of their normal destination. They really travel
# there, so this captures the offal energy AND the shorter commute AND the fact
# that they no longer route past Berwick Bank (so displacement cannot touch them).
if (RUN_OFFAL_CELL) {
  cat(sprintf("\n2c: %.0f%% of adults fly to an offal cell near the colony; sweeping the amount.\n",
              100 * OFFAL_CELL_ACCESS_FRAC))
  for (kg in OFFAL_CELL_KG) {
    key <- paste0("offalcell_", kg, "kg")
    if (kg == 0) {
      # No offal: nobody is sent anywhere -- a clean "with BB, no intervention" point.
      results[[key]] <- run_config(WF_WITH_BB, key, "offalcell_kg", kg)
    } else {
      # Site the cell on the most-visited reachable sea cell within
      # OFFAL_CELL_RADIUS_M of the colony, and load it with kg of offal at 9 kJ/g.
      pt <- make_point_prey(
        seamask = seamask, center = COLONY_POINT,
        Pmedian_value = CALIBRATED_PMEDIAN, energy_prey_model = spdat$energy_prey,
        BrdData = BrdData, min_distance = 0, max_distance = OFFAL_CELL_RADIUS_M,
        target_mass_g = kg * 1000, offal_energy_density = OFFAL_KJ_PER_G)
      results[[key]] <- run_config(
        WF_WITH_BB, key, "offalcell_kg", kg,
        PreyMap = pt$PreyMap, EnergyMap = pt$EnergyMap,
        offal_frac = OFFAL_CELL_ACCESS_FRAC,
        offal_biomass_g = 0,          # 0 -> use the CELL's prey, not a blanket override
        offal_cell = pt$target_cell)  # birds are sent here every trip
      if (kg == OFFAL_CELL_KG[which(OFFAL_CELL_KG > 0)[1]]) {
        saveRDS(pt, "outputs/offal_cell_geometry.rds")   # site is identical across kg
      }
    }
    save_progress()
  }
}

# =============================================================================
# 4. Analysis: how much offal restores the target?
# =============================================================================
res_df <- dplyr::bind_rows(results)
readr::write_csv(res_df, "outputs/bb_compensation_results.csv")
cat("\n=== All configurations ===\n"); print(as.data.frame(res_df), row.names = FALSE)

# Interpolate the offal amount at which each metric reaches the WITHOUT_BB target.
# Both mechanisms now titrate an AMOUNT (kg), under different access assumptions.
mech_desc <- c(
  spatial_kg       = "total kg dumped near the colony; birds find it randomly (BrdData)",
  perbird_offal_kg = sprintf("kg per bird per trip; %.0f%% feed on offal WHERE THEY ALREADY FORAGE (food effect only)",
                             100 * ACCESS_FRAC_FIXED),
  offalcell_kg     = sprintf("kg in a cell near the colony; %.0f%% of adults FLY THERE (food + shorter trip + no displacement)",
                             100 * OFFAL_CELL_ACCESS_FRAC)
)
mech_desc <- mech_desc[names(mech_desc) %in% unique(res_df$mechanism)]
# Interpolate safely: a metric that never varies (e.g. survival pinned at 1.0 --
# no adult ever dies at calibrated prey) has no curve to invert, so approx()
# would error. Report it as "not binding" instead.
# How much offal is needed to reach `target`? Guards against three traps:
#  (a) the metric has no real deficit to close (e.g. survival, which BB does not
#      measurably affect) -- interpolating its noise yields a spurious answer;
#  (b) the metric plateaus below the target -- more offal cannot help, so
#      "extend the sweep" would be wrong advice;
#  (c) the metric genuinely never reaches the target within the swept range.
# `deficit` is the gap this metric actually has to close (target - with_BB).
need_amount <- function(metric, amount, target, deficit = NULL, noise = 0.01) {
  ok <- is.finite(metric) & is.finite(amount)
  metric <- metric[ok]; amount <- amount[ok]
  if (length(metric) < 2) return(list(value = NA_real_, note = "too few points"))

  # (a) no meaningful deficit -> nothing to restore; any fit would be noise
  if (!is.null(deficit) && abs(deficit) < noise) {
    return(list(value = NA_real_,
                note = sprintf("no deficit to close (BB effect %+.4f, within noise) - not binding", deficit)))
  }
  if (target >= min(metric) && target <= max(metric)) {
    return(list(value = approx(metric, amount, xout = target, ties = mean)$y, note = NA_character_))
  }
  # (b) plateaued below target? compare the last third of the sweep to its peak
  o <- order(amount); m <- metric[o]; a <- amount[o]
  tail_n <- max(2, ceiling(length(m) / 3))
  tail_m <- utils::tail(m, tail_n)
  plateaued <- (max(m) - min(tail_m)) < noise && max(m) < target
  if (plateaued) {
    return(list(value = NA_real_,
                note = sprintf("PLATEAUS at %.4f, short of target %.4f - more offal cannot close the gap (closes %.0f%% of it)",
                               max(m), target,
                               100 * (max(m) - m[1]) / (target - m[1]))))
  }
  list(value = NA_real_, note = "target not reached within swept range - extend the sweep")
}

# BB's actual deficit in each metric -- a metric with no deficit is not binding.
deficit_surv <- target_surv - results$with_bb$survival
deficit_prod <- target_prod - results$with_bb$productivity
cat(sprintf("\nBB deficit to close: survival %+.4f | productivity %+.4f\n",
            deficit_surv, deficit_prod))

for (mech in names(mech_desc)) {
  d <- dplyr::filter(res_df, mechanism == mech) %>% dplyr::arrange(offal_amount)
  if (nrow(d) < 2) next
  s <- need_amount(d$survival,     d$offal_amount, target_surv, deficit = deficit_surv)
  p <- need_amount(d$productivity, d$offal_amount, target_prod, deficit = deficit_prod)
  fmt <- function(r) if (is.na(r$value)) r$note else paste(round(r$value, 3), "kg")
  cat(sprintf("\n[%s]  (%s)\n", mech, mech_desc[[mech]]))
  cat(sprintf("   to restore survival    : %s\n", fmt(s)))
  cat(sprintf("   to restore productivity: %s\n", fmt(p)))
  # Only metrics with a real deficit AND a resolved amount can be binding.
  binding <- suppressWarnings(max(c(s$value, p$value), na.rm = TRUE))
  if (is.finite(binding)) {
    cat(sprintf("   => OFFAL NEEDED TO OFFSET BERWICK BANK: %.3f kg (binding metric)\n", binding))
  } else {
    cat("   => OFFAL CANNOT OFFSET BERWICK BANK at this access fraction.\n")
    cat("      The limiting factor is how many birds reach the offal, not how much\n")
    cat("      is provided -- raise OFFAL_CELL_ACCESS_FRAC rather than the amount.\n")
  }
}

# =============================================================================
# 5. Dose-response plot
# =============================================================================
library(ggplot2)
plot_df <- res_df %>%
  dplyr::filter(mechanism %in% names(mech_desc)) %>%
  tidyr::pivot_longer(c(survival, productivity), names_to = "metric", values_to = "value")
p <- ggplot(plot_df, aes(offal_amount, value, colour = metric)) +
  geom_point() + geom_line() +
  geom_hline(data = data.frame(metric = c("survival","productivity"),
                               y = c(target_surv, target_prod)),
             aes(yintercept = y, colour = metric), lty = 2) +
  facet_wrap(~mechanism, scales = "free_x") +
  labs(title = "Offal needed to offset Berwick Bank",
       subtitle = sprintf("dashed = WITHOUT-BB target | spatial: total kg, random access | perbird: kg/bird/trip, %.0f%% always feed",
                          100 * ACCESS_FRAC_FIXED),
       x = "Offal amount (kg)", y = "Value")
ggsave("outputs/bb_compensation_doseresponse.png", p, width = 10, height = 5, dpi = 150)

cat("\nDONE. Results: outputs/bb_compensation_results.{rds,csv}; plot: outputs/bb_compensation_doseresponse.png\n")
