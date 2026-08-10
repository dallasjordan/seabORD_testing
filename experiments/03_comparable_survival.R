################################################################################
## Experiment 03: adult annual survival on a common reference
##
## seabORD's AdultsSurvivingYr is a real proportion, but the per-bird probability
## is computed against the SAME CONFIG's own base season:
##
##     meanbm     = mean(that config's base-season BM_adult)
##     P(survive) = ilogit( logit(basesurv) + (BM_adult - meanbm) * beta )
##
## So it measures the ORD effect WITHIN a config. Any intervention present in
## BOTH seasons -- offal is dropped regardless of Berwick Bank -- lifts base and
## scen equally and cancels out, which is why survival looked flat across the
## sweep.
##
## This recomputes survival for every config against ONE fixed reference mass,
## so the numbers are comparable BETWEEN configs.
##
## Runs on saved output only -- no re-simulation.
################################################################################

source("experiments/_setup_inputs.R")

RAW <- sprintf("outputs/bb_compensation_2c_%d_raw.rds", COLONY_PAIRS)
if (!file.exists(RAW)) {
  stop("No raw output for a ", COLONY_PAIRS, "-pair colony (", RAW, "). ",
       "Run 02_bb_compensation_experiment.R first, or set COLONY_PAIRS in ",
       "_setup_inputs.R to match an existing run.")
}
raw <- readRDS(RAW)
cat("Reading:", RAW, "\n")

ilogit <- function(x) exp(x) / (1 + exp(x))
logit  <- function(p) log(p / (1 - p))

beta    <- spdat$beta
bs_modr <- spdat$basesurv_modr
bs_good <- spdat$basesurv_good
bs_poor <- spdat$basesurv_poor
ml_modr <- spdat$massloss_modr / 100
ml_good <- spdat$massloss_good / 100
ml_poor <- spdat$massloss_poor / 100

cat(sprintf("KI: beta=%g | basesurv poor/modr/good = %.2f/%.2f/%.2f | massloss %.0f/%.0f/%.0f%%\n",
            beta, bs_poor, bs_modr, bs_good, 100*ml_poor, 100*ml_modr, 100*ml_good))

# --- End-of-season adult mass per config --------------------------------------
grab <- function(nm, season) {
  y <- dplyr::bind_rows(raw[[nm]]$output_y0) %>%
    dplyr::filter(Season == season, !is.na(BM_adult.mn))
  if (nrow(y) == 0) return(NULL)
  tibble::tibble(config = nm, season = season,
                 mass_t0 = mean(y$BM_adult_t0.mn, na.rm = TRUE),
                 mass_end = mean(y$BM_adult.mn,   na.rm = TRUE),
                 native_surv = mean(y$AdultsSurvivingYr, na.rm = TRUE))
}

dat <- dplyr::bind_rows(lapply(names(raw), grab, season = "scen"))
base_ref <- grab("without_BB", "base")
if (is.null(base_ref)) stop("without_BB base season not found -- cannot set the reference.")
REF <- base_ref$mass_end
cat(sprintf("\nCommon reference mass = without_BB base season mean = %.2f g\n\n", REF))

dat <- dat %>%
  dplyr::mutate(
    mass_loss   = (mass_t0 - mass_end) / mass_t0,
    # Same formula seabORD uses, but every config against the SAME mass.
    surv_common = ilogit(logit(bs_modr) + (mass_end - REF) * beta),
    # Independent cross-check: interpolate basesurv from the mass-loss bands.
    surv_band   = approx(x = c(ml_good, ml_modr, ml_poor),
                         y = c(bs_good, bs_modr, bs_poor),
                         xout = mass_loss, rule = 2)$y
  )

out <- dat %>% dplyr::select(config, mass_end, mass_loss, native_surv, surv_common, surv_band)
cat("=== Adult annual survival, recomputed on a common reference ===\n")
cat("native_surv = seabORD's AdultsSurvivingYr (each config vs its own base)\n")
cat("surv_common = same formula, all configs vs one reference mass\n")
cat("surv_band   = independent check, basesurv interpolated from mass loss\n\n")
print(as.data.frame(out %>% dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, 4)))),
      row.names = FALSE)

# --- Headline contrasts -------------------------------------------------------
gv <- function(cfg, col) { v <- out[[col]][out$config == cfg]; if (length(v)) v else NA_real_ }
if (!is.na(gv("with_BB", "surv_common"))) {
  cat(sprintf("\nBB's survival cost : %+.4f (common ref) | %+.4f (band method)\n",
              gv("with_BB","surv_common") - gv("without_BB","surv_common"),
              gv("with_BB","surv_band")   - gv("without_BB","surv_band")))
}
offal <- out[grepl("^offalcell_", out$config), ]
if (nrow(offal) > 0) {
  cat("\nOffal effect vs with_BB (common reference):\n")
  for (i in seq_len(nrow(offal))) {
    cat(sprintf("  %-18s surv %.4f (%+.4f vs with_BB) | mass loss %.4f\n",
                offal$config[i], offal$surv_common[i],
                offal$surv_common[i] - gv("with_BB","surv_common"), offal$mass_loss[i]))
  }
  cat(sprintf("\nTarget (without_BB) survival on common reference: %.4f\n",
              gv("without_BB","surv_common")))
}

OUT_CSV <- sprintf("outputs/comparable_survival_%d.csv", COLONY_PAIRS)
readr::write_csv(out, OUT_CSV)
cat(sprintf("\nSaved: %s\n", OUT_CSV))
cat("NOTE: computed from MEAN mass, so it ignores within-population spread\n")
cat("      (mean of survival != survival of the mean). Fine for comparing\n")
cat("      configs; do not quote as an absolute colony survival rate.\n")

