################################################################################
## Experiment 03: calibrate Pmedian for the current configuration
##
## Pmedian (baseline prey density, g/cell) should be CALIBRATED so that the
## no-windfarm baseline reproduces realistic "moderate" conditions for Isle of
## May kittiwakes. Following the F_exampleKI_calib vignette, we:
##   1. Run BASELINE-only (no ORDs) across a sweep of prey values (one per rep).
##   2. Measure adult mass loss and chicks-per-nest at each prey value.
##   3. Pick the prey value(s) where BOTH sit in the "moderate" bounds:
##        - Adult mass loss : 10%  (bounds 9-11%)
##        - Chicks per nest  : 0.50 (bounds 0.45-0.55)
##
## Uses the SAME inputs as the scenario run (02) so the calibrated value is valid
## there. Runs in switches$modelmode = "calibration" (base season only, no ORDs).
##
## RUNTIME: N_PREY replicates x full season. Budget ~5-10 min per prey value at
## 10% population -> a 15-value sweep is roughly 1.5-2.5 hours. Reduce N_PREY or
## CALIB_POP_FRACTION for a faster first pass, or run it in the background.
################################################################################

.libPaths(c("C:/Users/dallas.jordan/AppData/Local/R/win-library/4.6", .libPaths()))
suppressPackageStartupMessages({
  library(seabORD); library(raster); library(sf); library(dplyr)
  library(ggplot2); library(gridExtra)
})
setwd("C:/Users/dallas.jordan/OneDrive - SLR Consulting/Projects/seabORD_testing")
dir.create("outputs", showWarnings = FALSE)

# =============================================================================
# 1. Load inputs (SAME as experiment 02)
# =============================================================================
data("example_1_lists");        data("seamask_3035_example"); data("BrdData_example")
data("frgcompdata_example");    data("UK9004171_bysea_3035"); data("FlightGridcorrection_3035")
data("energeticsandpreydata");  data("spacoordinates");       data("spalist")
data("ORDpoly_example")   # calibration ignores ORDs, but seabord() requires the argument

rebuild_raster <- function(rlist, name = NULL) {
  md <- rlist$metadata
  r <- raster::setValues(raster::raster(nrows = md$n_rows, ncols = md$n_cols,
        xmn = md$x_min, xmx = md$x_max, ymn = md$y_min, ymx = md$y_max, crs = md$crs),
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
spadat2      <- tibble::as_tibble(dplyr::filter(spalist,        SITE_CODE == SPA_CODE))
spdat        <- dplyr::filter(energeticsandpreydata, Code == "KI")
FlightGridcorrection <- FlightGridcorrection_3035
names(BrdData) <- paste(SPA_CODE, "KI", sep = "_")

# =============================================================================
# 2. Calibration knobs
# =============================================================================
PREY_MIN  <- 165     # lower end of the prey sweep (g/cell)
PREY_MAX  <- 185     # upper end
N_PREY    <- 11     # number of prey values = number of replicates (one rep each)
CALIB_POP_FRACTION <- 0.1   # 10% of the population (vignette default for calibration)

# Moderate-condition targets & bounds (Isle of May kittiwake, per the vignette)
MASS_TARGET <- 0.10; MASS_LO <- 0.09; MASS_HI <- 0.11   # adult mass loss (fraction)
PROD_TARGET <- 0.50; PROD_LO <- 0.45; PROD_HI <- 0.55   # chicks per nest

prey_sweep <- round(seq(PREY_MIN, PREY_MAX, length.out = N_PREY))

Par      <- example_1_lists$Par
modPar   <- example_1_lists$modPar
ordPar   <- example_1_lists$ordPar
switches <- example_1_lists$switches

switches$modelmode  <- "calibration"          # base season only, no ORDs
Par$Nscalefactor    <- CALIB_POP_FRACTION
Par$Pmedian         <- prey_sweep             # one prey value per replicate
modPar$Nreplicates  <- length(prey_sweep)     # MUST equal length(Pmedian)
ordPar$include_ORDs <- NULL                   # not used in calibration

cat("Calibration sweep:", paste(range(prey_sweep), collapse = " to "),
    "g/cell in", N_PREY, "steps;", round(100*CALIB_POP_FRACTION), "% population.\n")
cat("Prey values:", paste(prey_sweep, collapse = ", "), "\n\n")

# =============================================================================
# 3. Run the calibration (one seabord call does all replicates)
# =============================================================================
t0 <- Sys.time()
cal <- seabord(
  Par = Par, modPar = modPar, ordPar = ordPar, switches = switches,
  seamask = seamask, spadat1 = spadat1, spadat2 = spadat2, spdat = spdat,
  BrdData = BrdData, FrgCompData = FrgCompData, fltdist_base = fltdist_base,
  FlightGridcorrection = FlightGridcorrection, ORDpoly = ORDpoly_example,
  PreyMap = NULL, EnergyMap = NULL
)
cat(sprintf("Calibration finished in %.1f min\n", as.numeric(Sys.time() - t0, units = "mins")))
saveRDS(cal, "outputs/calibration_output.rds")

# =============================================================================
# 4. Extract demographics per prey value (end-of-season)
# =============================================================================
# Adult mass loss = (start mass - end mass) / start mass, at the final timestep.
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
# 5. Find the calibrated prey value(s)
# =============================================================================
# Interpolate the prey at which each metric hits its target, and flag any prey
# values where BOTH metrics fall inside their moderate bounds.
# Interpolate the prey value (x) at which a metric (y) equals `target`.
# Sort by the METRIC (y) so this works whether the relationship rises
# (chicks per nest) or falls (adult mass loss) with prey.
prey_at <- function(x, y, target) {
  ok <- is.finite(x) & is.finite(y); x <- x[ok]; y <- y[ok]
  if (target < min(y) || target > max(y)) return(NA_real_)
  o <- order(y); approx(y[o], x[o], xout = target)$y
}
prey_mass <- prey_at(cal_df$Prey, cal_df$mass_loss,    MASS_TARGET)
prey_prod <- prey_at(cal_df$Prey, cal_df$ChicksPerNest, PROD_TARGET)

both_ok <- cal_df %>%
  dplyr::filter(mass_loss >= MASS_LO, mass_loss <= MASS_HI,
                ChicksPerNest >= PROD_LO, ChicksPerNest <= PROD_HI)

cat("\n=== Calibrated Pmedian ===\n")
cat(sprintf("Prey giving 10%% adult mass loss : %s\n",
            ifelse(is.na(prey_mass), "outside swept range", sprintf("%.0f g/cell", prey_mass))))
cat(sprintf("Prey giving 0.50 chicks/nest    : %s\n",
            ifelse(is.na(prey_prod), "outside swept range", sprintf("%.0f g/cell", prey_prod))))
if (!is.na(prey_mass) && !is.na(prey_prod)) {
  cat(sprintf("=> Suggested Pmedian (midpoint) : %.0f g/cell\n", mean(c(prey_mass, prey_prod))))
}
if (nrow(both_ok) > 0) {
  cat("Prey values inside BOTH moderate bounds:", paste(both_ok$Prey, collapse = ", "), "\n")
} else {
  cat("(No single swept value fell inside both bounds -- use the interpolated midpoint,\n")
  cat(" or refine the sweep around it and re-run.)\n")
}

# =============================================================================
# 6. Plot (mirrors the F vignette)
# =============================================================================
pAd <- ggplot(cal_df, aes(Prey, mass_loss)) +
  geom_hline(yintercept = c(MASS_LO, MASS_HI), lty = 2, colour = "darkgrey") +
  geom_point(colour = "red") + geom_line(colour = "red") +
  labs(y = "Adult mass loss (fraction)", x = "Prey (g/cell)",
       title = "Calibration: adult mass loss vs prey")
pCh <- ggplot(cal_df, aes(Prey, ChicksPerNest)) +
  geom_hline(yintercept = c(PROD_LO, PROD_HI), lty = 2, colour = "darkgrey") +
  geom_point(colour = "red") + geom_line(colour = "red") +
  labs(y = "Chicks per nest", x = "Prey (g/cell)",
       title = "Calibration: chicks per nest vs prey")
ggsave("outputs/calibration_pmedian.png",
       gridExtra::arrangeGrob(pAd, pCh, ncol = 1), width = 8, height = 8, dpi = 150)
cat("\nPlot saved to outputs/calibration_pmedian.png\n")
cat("DONE. Use the suggested Pmedian in experiment 02 (FIXED_PMEDIAN).\n")
