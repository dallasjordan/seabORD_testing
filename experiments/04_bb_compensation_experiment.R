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
POP_FRACTION <- 0.05     # keep small for a first pass; raise for final inference
N_REPLICATES <- 3        # survival/productivity are stochastic -> use >=3, ideally >=20

OFFAL_KJ_PER_G <- 9      # offal quality (kJ/g available to kittiwakes)

# Windfarm configurations (positional toggles resolved by load_windfarms)
WF_WITHOUT_BB <- c(INCAP = TRUE, SEAGREEN = TRUE, NEART = TRUE, BERWICK = FALSE)
WF_WITH_BB    <- c(INCAP = TRUE, SEAGREEN = TRUE, NEART = TRUE, BERWICK = TRUE)

# Offal near the colony (spatial mechanism, 3a)
COLONY_RADIUS_M <- 20000                     # disc radius around the Isle of May
SPATIAL_OFFAL_KG <- c(0, 2000, 5000, 10000, 20000)   # biomass sweep (kg)

# Per-bird access (mechanism 3b)
ACCESS_FRAC <- c(0, 0.10, 0.25, 0.47, 0.70)  # fraction-of-population sweep

# Offal biomass made available to each per-bird accessor per trip (>> saturation)
OFFAL_BIOMASS_PERBIRD_G <- 2000 * 1000

colony_point <- COLONY_POINT   # from _setup_inputs.R

# =============================================================================
# Helpers
# =============================================================================
# Pull scen-season survival & productivity, averaged over replicates.
get_metrics <- function(res) {
  a <- dplyr::bind_rows(res$output_a0) %>%
    dplyr::filter(Season == "scen", !is.na(t)) %>%
    dplyr::group_by(Rep) %>% dplyr::filter(t == max(t)) %>% dplyr::ungroup() %>%
    dplyr::mutate(surv = N_alive_ad / (N_alive_ad + N_dead_ad))
  c0 <- dplyr::bind_rows(res$output_c0) %>%
    dplyr::filter(Season == "scen", !is.na(t)) %>%
    dplyr::group_by(Rep) %>% dplyr::filter(t == max(t)) %>% dplyr::ungroup()
  tibble::tibble(survival     = mean(a$surv, na.rm = TRUE),
                 productivity = mean(c0$ChicksPerNest, na.rm = TRUE))
}

# Run one full configuration and return its metrics + a label row.
run_config <- function(windfarms, label, mechanism, offal_amount,
                       PreyMap = NULL, EnergyMap = NULL, offal_frac = 0) {
  message(sprintf("--- %s (%s = %s) ---", label, mechanism, offal_amount))
  wf <- load_windfarms(WINDFARM_SHP, target_crs = raster::crs(seamask),
                       include = names(windfarms)[windfarms])
  Par_i <- Par
  Par_i$Nscalefactor       <- POP_FRACTION
  Par_i$Pmedian            <- rep(CALIBRATED_PMEDIAN, N_REPLICATES)
  Par_i$PreyType           <- if (is.null(PreyMap)) "Uniform" else "Map"
  Par_i$OffalAccessFrac    <- offal_frac
  Par_i$OffalBiomass_g     <- OFFAL_BIOMASS_PERBIRD_G
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
  message(sprintf("   survival=%.4f productivity=%.4f (%.1f min)",
                  m$survival, m$productivity, m$minutes))
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
cat(sprintf("\nTARGET (without BB): survival=%.4f productivity=%.4f\n", target_surv, target_prod))
cat(sprintf("WITH BB           : survival=%.4f productivity=%.4f  (the gap to close)\n\n",
            results$with_bb$survival, results$with_bb$productivity))

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
# 3b. Per-bird offal access -- sweep the access fraction
# =============================================================================
for (fr in ACCESS_FRAC) {
  key <- paste0("access_", round(100 * fr), "pct")
  results[[key]] <- run_config(WF_WITH_BB, key, "access_frac", fr, offal_frac = fr)
  save_progress()
}

# =============================================================================
# 4. Analysis: how much offal restores the target?
# =============================================================================
res_df <- dplyr::bind_rows(results)
readr::write_csv(res_df, "outputs/bb_compensation_results.csv")
cat("\n=== All configurations ===\n"); print(as.data.frame(res_df), row.names = FALSE)

# Interpolate the offal amount at which each metric reaches the WITHOUT_BB target.
amount_at <- function(df, target) {
  df <- dplyr::arrange(df, offal_amount)
  if (target < min(df[[3]]) || target > max(df[[3]])) return(NA_real_)  # 3rd col = metric
  approx(df[[3]], df$offal_amount, xout = target)$y
}
for (mech in c("spatial_kg", "access_frac")) {
  d <- dplyr::filter(res_df, mechanism == mech)
  ds <- dplyr::select(d, offal_amount, survival);     names(ds)[2] <- "m"; ds <- dplyr::rename(ds, val = m)
  dp <- dplyr::select(d, offal_amount, productivity); names(dp)[2] <- "m"; dp <- dplyr::rename(dp, val = m)
  need_surv <- approx(ds$val, ds$offal_amount, xout = target_surv, ties = mean)$y
  need_prod <- approx(dp$val, dp$offal_amount, xout = target_prod, ties = mean)$y
  unit <- if (mech == "spatial_kg") "kg offal" else "fraction access"
  cat(sprintf("\n[%s] to restore survival  : %s %s\n", mech,
              ifelse(is.na(need_surv), "outside swept range", round(need_surv, 3)), unit))
  cat(sprintf("[%s] to restore productivity: %s %s\n", mech,
              ifelse(is.na(need_prod), "outside swept range", round(need_prod, 3)), unit))
  cat(sprintf("[%s] => offal needed to offset BB: %s %s (max of the two)\n", mech,
              ifelse(any(is.na(c(need_surv,need_prod))), "extend the sweep",
                     round(max(need_surv, need_prod, na.rm = TRUE), 3)), unit))
}

# =============================================================================
# 5. Dose-response plot
# =============================================================================
library(ggplot2)
plot_df <- res_df %>% dplyr::filter(mechanism %in% c("spatial_kg", "access_frac")) %>%
  tidyr::pivot_longer(c(survival, productivity), names_to = "metric", values_to = "value")
p <- ggplot(plot_df, aes(offal_amount, value, colour = metric)) +
  geom_point() + geom_line() +
  geom_hline(data = data.frame(metric = c("survival","productivity"),
                               y = c(target_surv, target_prod)),
             aes(yintercept = y, colour = metric), lty = 2) +
  facet_wrap(~mechanism, scales = "free_x") +
  labs(title = "Offal needed to offset Berwick Bank",
       subtitle = "dashed = WITHOUT-BB target; x = offal amount (kg or access fraction)",
       x = "Offal amount", y = "Value")
ggsave("outputs/bb_compensation_doseresponse.png", p, width = 10, height = 5, dpi = 150)

cat("\nDONE. Results: outputs/bb_compensation_results.{rds,csv}; plot: outputs/bb_compensation_doseresponse.png\n")
