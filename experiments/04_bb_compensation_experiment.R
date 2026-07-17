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
POP_FRACTION <- 0.1     # keep small for a first pass; raise for final inference
N_REPLICATES <- 10        # survival/productivity are stochastic -> use >=3, ideally >=20

OFFAL_KJ_PER_G <- 9      # offal quality (kJ/g available to kittiwakes)

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
                       offal_frac = 0, offal_biomass_g = 0) {
  message(sprintf("--- %s (%s = %s) ---", label, mechanism, offal_amount))
  wf <- load_windfarms(WINDFARM_SHP, target_crs = raster::crs(seamask),
                       include = names(windfarms)[windfarms])
  Par_i <- Par
  Par_i$Nscalefactor       <- POP_FRACTION
  Par_i$Pmedian            <- rep(CALIBRATED_PMEDIAN, N_REPLICATES)
  Par_i$PreyType           <- if (is.null(PreyMap)) "Uniform" else "Map"
  Par_i$OffalAccessFrac    <- offal_frac       # 0 = per-bird offal access off
  Par_i$OffalBiomass_g     <- offal_biomass_g  # offal available per accessing bird per trip
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
  m
}

results <- list()
save_progress <- function() saveRDS(dplyr::bind_rows(results), "outputs/bb_compensation_results.rds")

# =============================================================================
# 1 & 2. Baseline configurations
# =============================================================================
results$without_bb <- run_config(WF_WITHOUT_BB, "without_BB", "none", 0); save_progress()
results$with_bb    <- run_config(WF_WITH_BB,    "with_BB",    "none", 0); save_progress()

target_surv <- results$without_bb$survival
target_prod <- results$without_bb$productivity
cat(sprintf("\nTARGET (without BB): annual survival=%.4f  mass loss=%.3f  productivity=%.4f\n",
            target_surv, results$without_bb$mass_loss, target_prod))
cat(sprintf("WITH BB           : annual survival=%.4f  mass loss=%.3f  productivity=%.4f\n",
            results$with_bb$survival, results$with_bb$mass_loss, results$with_bb$productivity))
cat(sprintf("BB's cost         : survival %+.4f   productivity %+.4f   (the gap to close)\n\n",
            results$with_bb$survival - target_surv,
            results$with_bb$productivity - target_prod))

# =============================================================================
# 3a. Spatial offal near the colony -- sweep biomass
# =============================================================================
for (kg in SPATIAL_OFFAL_KG) {
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
# 3b. Offal cell fed on by a GUARANTEED share of birds -- sweep the offal AMOUNT
# =============================================================================
# ACCESS_FRAC_FIXED (47%) of adults feed on the offal every trip, regardless of
# where they would otherwise forage. We sweep how much offal is available to each
# of them per trip, and find the amount that restores the WITHOUT_BB target.
cat(sprintf("\n3b assumes %.0f%% of adults always feed on the offal; sweeping the amount.\n",
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

# =============================================================================
# 4. Analysis: how much offal restores the target?
# =============================================================================
res_df <- dplyr::bind_rows(results)
readr::write_csv(res_df, "outputs/bb_compensation_results.csv")
cat("\n=== All configurations ===\n"); print(as.data.frame(res_df), row.names = FALSE)

# Interpolate the offal amount at which each metric reaches the WITHOUT_BB target.
# Both mechanisms now titrate an AMOUNT (kg), under different access assumptions.
mech_desc <- c(
  spatial_kg      = "total kg dumped near the colony; birds find it randomly (BrdData)",
  perbird_offal_kg = sprintf("kg per accessing bird per trip; %.0f%% of adults always feed on it",
                             100 * ACCESS_FRAC_FIXED)
)
# Interpolate safely: a metric that never varies (e.g. survival pinned at 1.0 --
# no adult ever dies at calibrated prey) has no curve to invert, so approx()
# would error. Report it as "not binding" instead.
need_amount <- function(metric, amount, target) {
  ok <- is.finite(metric) & is.finite(amount)
  metric <- metric[ok]; amount <- amount[ok]
  if (length(unique(metric)) < 2) {
    return(list(value = NA_real_,
                note = if (isTRUE(all.equal(unique(metric)[1], target)))
                         "already at target (not binding)" else "no variation - cannot invert"))
  }
  if (target < min(metric) || target > max(metric)) {
    return(list(value = NA_real_, note = "outside swept range - extend the sweep"))
  }
  list(value = approx(metric, amount, xout = target, ties = mean)$y, note = NA_character_)
}

for (mech in names(mech_desc)) {
  d <- dplyr::filter(res_df, mechanism == mech) %>% dplyr::arrange(offal_amount)
  if (nrow(d) < 2) next
  s <- need_amount(d$survival,     d$offal_amount, target_surv)
  p <- need_amount(d$productivity, d$offal_amount, target_prod)
  fmt <- function(r) if (is.na(r$value)) r$note else paste(round(r$value, 3), "kg")
  cat(sprintf("\n[%s]  (%s)\n", mech, mech_desc[[mech]]))
  cat(sprintf("   to restore survival    : %s\n", fmt(s)))
  cat(sprintf("   to restore productivity: %s\n", fmt(p)))
  binding <- suppressWarnings(max(c(s$value, p$value), na.rm = TRUE))
  cat(sprintf("   => OFFAL NEEDED TO OFFSET BERWICK BANK: %s\n",
              if (is.finite(binding)) paste(round(binding, 3), "kg (binding metric)")
              else "not resolved - see notes above"))
}

# =============================================================================
# 5. Dose-response plot
# =============================================================================
library(ggplot2)
plot_df <- res_df %>%
  dplyr::filter(mechanism %in% c("spatial_kg", "perbird_offal_kg")) %>%
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
