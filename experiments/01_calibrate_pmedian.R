################################################################################
## Experiment 01: calibrate Pmedian
##
## Pmedian (baseline prey density, g/cell) is calibrated so the no-windfarm
## baseline reproduces "moderate" conditions for Isle of May kittiwakes:
##   1. Run baseline-only (no ORDs) across a sweep of prey values, one per rep.
##   2. Measure adult mass loss and chicks per nest at each value.
##   3. Take the value(s) where BOTH sit inside their moderate bounds.
##
## Runs in switches$modelmode = "calibration" (base season only, no ORDs).
## Set the result as CALIBRATED_PMEDIAN in _setup_inputs.R.
##
## NOTE: calibration inherits COLONY_PAIRS from _setup_inputs.R, so re-run this
## if the colony size changes. The effect should be small -- colony size reaches
## the model only through the competition term, which is near-inert for
## kittiwake (IR_half_b = 0.02) -- but the current value was calibrated at a
## different population and has not been re-derived.
##
## RUNTIME: one base season per prey value, so roughly half a sweep replicate.
## At 10% of 6068 pairs (1214 adults) budget ~13 min each -> ~2.5 h for 11 values.
##
## If no value falls inside BOTH bounds, widen PREY_MIN/PREY_MAX rather than
## averaging the two reference crossings printed at the end -- the two metrics
## have very different slopes and the average can sit outside both bands.
################################################################################

source("experiments/_setup_inputs.R")
suppressPackageStartupMessages({ library(ggplot2); library(gridExtra) })
data("ORDpoly_example")   # calibration ignores ORDs, but seabord() requires it

# =============================================================================
# Config
# =============================================================================
PREY_MIN <- 150     # lower end of the prey sweep (g/cell)
PREY_MAX <- 200     # upper end
N_PREY   <- 11      # number of prey values = number of replicates

# Moderate-condition targets and bounds (Isle of May kittiwake)
MASS_TARGET <- 0.10; MASS_LO <- 0.09; MASS_HI <- 0.11   # adult mass loss
PROD_TARGET <- 0.50; PROD_LO <- 0.45; PROD_HI <- 0.55   # chicks per nest

prey_sweep <- round(seq(PREY_MIN, PREY_MAX, length.out = N_PREY))

switches$modelmode  <- "calibration"          # base season only, no ORDs
Par$Pmedian         <- prey_sweep             # one prey value per replicate
modPar$Nreplicates  <- length(prey_sweep)     # MUST equal length(Pmedian)
ordPar$include_ORDs <- NULL

cat(sprintf("Calibration sweep: %d to %d g/cell in %d steps; %.0f%% of %d pairs.\n",
            min(prey_sweep), max(prey_sweep), N_PREY, 100 * POP_FRACTION, COLONY_PAIRS))
cat("Prey values:", paste(prey_sweep, collapse = ", "), "\n\n")

# =============================================================================
# Run (one seabord call does all replicates)
# =============================================================================
t0 <- Sys.time()
cal <- seabord(
  Par = Par, modPar = modPar, ordPar = ordPar, switches = switches,
  seamask = seamask, spadat1 = spadat1, spadat2 = spadat2, spdat = spdat,
  BrdData = BrdData, FrgCompData = FrgCompData, fltdist_base = fltdist_base,
  FlightGridcorrection = FlightGridcorrection, ORDpoly = ORDpoly_example,
  PreyMap = NULL, EnergyMap = NULL
)
cat(sprintf("Finished in %.1f min\n", as.numeric(Sys.time() - t0, units = "mins")))
saveRDS(cal, "outputs/calibration_output.rds")

# =============================================================================
# Demographics per prey value (end of season)
# =============================================================================
adult <- dplyr::bind_rows(cal$output_a0) %>%
  dplyr::filter(Season == "base", !is.na(t)) %>%
  dplyr::group_by(Rep) %>% dplyr::filter(t == max(t)) %>% dplyr::ungroup() %>%
  dplyr::mutate(mass_loss = (BM_adult_t0.mn - BM_adult.mn) / BM_adult_t0.mn) %>%
  dplyr::select(Rep, Prey, mass_loss)

chick <- dplyr::bind_rows(cal$output_c0) %>%
  dplyr::filter(Season == "base", !is.na(t)) %>%
  dplyr::group_by(Rep) %>% dplyr::filter(t == max(t)) %>% dplyr::ungroup() %>%
  dplyr::select(Rep, Prey, ChicksPerNest)

cal_df <- dplyr::left_join(adult, chick, by = c("Rep", "Prey")) %>% dplyr::arrange(Prey)
cat("\n=== Calibration results ===\n")
print(as.data.frame(cal_df), row.names = FALSE)

# =============================================================================
# Pick the calibrated value
# =============================================================================
# Interpolate the prey value (x) at which a metric (y) equals target. Sorts by
# the METRIC so it works whether the relationship rises (chicks per nest) or
# falls (adult mass loss) with prey.
prey_at <- function(x, y, target) {
  ok <- is.finite(x) & is.finite(y); x <- x[ok]; y <- y[ok]
  if (target < min(y) || target > max(y)) return(NA_real_)
  o <- order(y); approx(y[o], x[o], xout = target)$y
}
prey_mass <- prey_at(cal_df$Prey, cal_df$mass_loss,     MASS_TARGET)
prey_prod <- prey_at(cal_df$Prey, cal_df$ChicksPerNest, PROD_TARGET)

both_ok <- cal_df %>%
  dplyr::filter(mass_loss >= MASS_LO, mass_loss <= MASS_HI,
                ChicksPerNest >= PROD_LO, ChicksPerNest <= PROD_HI)

cat("\n=== Calibrated Pmedian ===\n")
# Reference only -- do NOT average these. Adult mass loss is shallow and stays in
# bounds across a wide prey range; chicks per nest is steep and only in bounds
# over a narrow window. Averaging can land where the steep metric is already out.
cat(sprintf("  (ref) prey at exactly 10%% adult mass loss : %s\n",
            ifelse(is.na(prey_mass), "outside swept range", sprintf("%.0f g/cell", prey_mass))))
cat(sprintf("  (ref) prey at exactly 0.50 chicks/nest    : %s\n",
            ifelse(is.na(prey_prod), "outside swept range", sprintf("%.0f g/cell", prey_prod))))

# The criterion: prey value(s) where BOTH metrics are inside their bounds.
if (nrow(both_ok) > 0) {
  cat(sprintf("\n=> CALIBRATED Pmedian: %s g/cell\n", paste(both_ok$Prey, collapse = ", ")))
  cat("   Set CALIBRATED_PMEDIAN in experiments/_setup_inputs.R to this value.\n")
} else {
  cat("\n=> No swept value fell inside BOTH bounds.\n")
  cat("   Refine the sweep around where chicks/nest crosses its target and re-run.\n")
}

# =============================================================================
# Plot
# =============================================================================
pAd <- ggplot(cal_df, aes(Prey, mass_loss)) +
  geom_hline(yintercept = c(MASS_LO, MASS_HI), lty = 2, colour = "darkgrey") +
  geom_point(colour = "red") + geom_line(colour = "red") +
  labs(y = "Adult mass loss (fraction)", x = "Prey (g/cell)",
       title = "Calibration: adult mass loss vs prey")
pCh <- ggplot(cal_df, aes(Prey, ChicksPerNest)) +
  geom_hline(yintercept = c(PROD_LO, PROD_HI), lty = 2, colour = "darkgrey") +
  geom_point(colour = "red") + geom_line(colour = "red") +
  labs(y = "Nests fledging a chick", x = "Prey (g/cell)",
       title = "Calibration: chicks per nest vs prey")
ggsave("outputs/calibration_pmedian.png",
       gridExtra::arrangeGrob(pAd, pCh, ncol = 1), width = 8, height = 8, dpi = 150)
cat("\nPlot: outputs/calibration_pmedian.png\n")
