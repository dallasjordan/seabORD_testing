################################################################################
## Experiment 05: recompute adult annual survival on a COMMON reference
##
## WHY THIS IS NEEDED
## seabORD's AdultsSurvivingYr is a genuine proportion of adults surviving
## (Survived_modr / npop), but the per-bird probability is computed against the
## SAME CONFIG's own base season:
##
##     if (season == "base") meanbm <- mean(base BM_adult)
##     else                  meanbm <- mean(YearBirds$base$BM_adult)
##     P(survive) = ilogit( logit(basesurv) + (BM_adult - meanbm) * beta )
##
## So it measures the ORD effect WITHIN a config. Any intervention present in
## BOTH seasons -- such as offal, which is dropped regardless of Berwick Bank --
## lifts base and scen equally and cancels out. That is why survival looked flat
## across the offal sweep.
##
## This script recomputes survival for every config against ONE fixed reference
## mass, so the numbers are comparable BETWEEN configs and can be reported as
## "BB's survival cost" and "the offal's survival benefit".
##
## Runs on saved output only -- no re-simulation required.
################################################################################

source("experiments/_setup_inputs.R")

RAW <- "outputs/bb_compensation_raw.rds"
stopifnot(file.exists(RAW))
raw <- readRDS(RAW)

ilogit <- function(x) exp(x) / (1 + exp(x))
logit  <- function(p) log(p / (1 - p))

beta      <- spdat$beta
bs_modr   <- spdat$basesurv_modr
bs_good   <- spdat$basesurv_good
bs_poor   <- spdat$basesurv_poor
ml_modr   <- spdat$massloss_modr / 100   # 10% -> 0.10
ml_good   <- spdat$massloss_good / 100
ml_poor   <- spdat$massloss_poor / 100

cat(sprintf("KI: beta=%g | basesurv poor/modr/good = %.2f/%.2f/%.2f | massloss %.0f/%.0f/%.0f%%\n",
            beta, bs_poor, bs_modr, bs_good, 100*ml_poor, 100*ml_modr, 100*ml_good))

# ---- Pull end-of-season adult mass per config/season -------------------------
grab <- function(nm, season) {
  y <- dplyr::bind_rows(raw[[nm]]$output_y0) %>%
    dplyr::filter(Season == season, !is.na(BM_adult.mn))
  if (nrow(y) == 0) return(NULL)
  tibble::tibble(config = nm, season = season,
                 mass_t0 = mean(y$BM_adult_t0.mn, na.rm = TRUE),
                 mass_end = mean(y$BM_adult.mn,   na.rm = TRUE),
                 native_surv = mean(y$AdultsSurvivingYr, na.rm = TRUE))
}

configs <- names(raw)
dat <- dplyr::bind_rows(lapply(configs, grab, season = "scen"))
base_ref <- grab("without_BB", "base")
if (is.null(base_ref)) stop("without_BB base season not found -- cannot set the reference.")
REF <- base_ref$mass_end
cat(sprintf("\nCommon reference mass = without_BB BASE season mean = %.2f g\n\n", REF))

# ---- (a) survival on a common reference --------------------------------------
# Same formula seabORD uses, but every config measured against the SAME mass.
dat <- dat %>%
  dplyr::mutate(
    mass_loss   = (mass_t0 - mass_end) / mass_t0,
    surv_common = ilogit(logit(bs_modr) + (mass_end - REF) * beta),
    # (b) independent cross-check: interpolate basesurv from the mass-loss bands
    surv_band   = approx(x = c(ml_good, ml_modr, ml_poor),
                         y = c(bs_good, bs_modr, bs_poor),
                         xout = mass_loss, rule = 2)$y
  )

out <- dat %>% dplyr::select(config, mass_end, mass_loss, native_surv, surv_common, surv_band)
cat("=== Adult annual survival, recomputed on a common reference ===\n")
cat("native_surv = seabORD's AdultsSurvivingYr (referenced to each config's own base)\n")
cat("surv_common = same formula, all configs vs the single reference mass\n")
cat("surv_band   = independent check: basesurv interpolated from mass loss\n\n")
print(as.data.frame(out %>% dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, 4)))),
      row.names = FALSE)

# ---- Headline contrasts ------------------------------------------------------
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

readr::write_csv(out, "outputs/comparable_survival.csv")
cat("\nSaved: outputs/comparable_survival.csv\n")
cat("NOTE: computed from MEAN mass, so it ignores within-population spread\n")
cat("      (mean of survival != survival of the mean). Fine for comparing\n")
cat("      configs; do not quote as an absolute colony survival rate.\n")
