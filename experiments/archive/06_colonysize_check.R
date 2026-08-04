################################################################################
## Experiment 06: does the colony-size correction invalidate the 2c sweep?
##
## WHY THIS EXISTS
## The 2c offal-cell sweep (experiment 04) ran with Par$Npairspercol = 2898, the
## package example value. The NatureScot Isle of May NNR Annual Report 2025 gives
## 6,068 apparently occupied nests (AON) for black-legged kittiwake -- 2.09x the
## package default. (2898 is closest to the 2016 count of 2,922; the colony has
## roughly doubled since, five consecutive years of increase.)
##
## Npairspercol enters the model in TWO places:
##
##  1. The kg -> access-fraction mapping in 04 (offal_access_frac divides the
##     deposit's kittiwake-days by the real colony). This is a pure RELABELLING:
##     the swept access fractions (0.025 -> 1.0) are unchanged, only the kg on the
##     x axis move, by the ratio 6068/2898 = 2.094. No re-simulation needed.
##
##  2. Competition. Par$Npairspercol -> thisRun$NBirdsRegion -> popbirdsperkm2
##     (functions-seabordmain.R:241,244), and seabord_daystep divides through it:
##     ComFactor = Birdsperkm2 / popbirdsperkm2 (functions-seabordday.R:173).
##     This one is NOT a relabelling -- it changes the simulation.
##
## The colony is only ~18% of the birds in the region (5,796 of 32,433; the rest
## is FrgCompData), so doubling it moves popbirdsperkm2 by ~+20%, not +100%. With
## IR_half_b = 0.02 competition is believed to be nearly inert, so the effect on
## productivity should be negligible -- but that is an inference, not a
## measurement, and the whole 2c sweep rests on it.
##
## THE TEST
## Re-run ONE config -- with_BB, no offal -- at Npairspercol = 6068 and compare
## productivity against the stored with_BB (0.4734 at 2898). Nothing else changes.
##
##   within noise  -> competition is inert. The 2c sweep stands as measured; only
##                    the kg axis is relabelled (x2.094). ~3 h spent instead of
##                    ~100 h re-running everything.
##   moves materially -> competition is NOT inert at this scale and the full 2c
##                    sweep must be re-run at the corrected colony size.
##
## "Materially" is judged against the quantity the study is trying to resolve:
## BB's productivity deficit, 0.0700. A shift that is a large share of that
## deficit contaminates the threshold; one that is a few percent of it does not.
##
## RUNTIME: ~2.1x the birds (1214 adults vs 580), so ~19 min/rep vs ~9. At 10
## reps expect ~3-4 h. Run in the background.
################################################################################

source("experiments/_setup_inputs.R")

# =============================================================================
# Config
# =============================================================================
POP_FRACTION  <- 0.1    # unchanged from 04, so the only difference is the colony
N_REPLICATES  <- 10     # go/no-go test, not a reportable baseline

# The correction under test. 6,068 AON, NatureScot Isle of May NNR Annual Report
# 2025, Table 10 (also section 2.3.3 and the Table 7 section totals -- three
# independent statements of the same figure).
NPAIRS_OLD <- 2898      # package example value, used by the 2c sweep
NPAIRS_NEW <- 6068      # Isle of May kittiwake AON, 2025

# Berwick Bank present, no offal -- the cleanest config to isolate colony size.
WF_WITH_BB <- c(INCAP = TRUE, SEAGREEN = TRUE, NEART = TRUE, BERWICK = TRUE)

OUT_RDS <- "outputs/colonysize_check.rds"

# Per-bird destinations are not needed here and would be 1214 birds x 30 days x
# 2 seasons x 10 reps of integers for nothing.
switches$saveperbirddest <- FALSE

# =============================================================================
# Metrics -- same definition as 04's get_metrics, so the two are comparable.
# survival_native is deliberately omitted: it is referenced to each config's own
# base season and is diagnostic only (see 04, and 05_comparable_survival.R).
# =============================================================================
get_metrics <- function(res) {
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

  p_rep <- per_rep(c0, "ChicksPerNest")
  m_rep <- per_rep(a,  "ml")
  tibble::tibble(productivity = mean(p_rep, na.rm = TRUE), productivity_se = se(p_rep),
                 mass_loss    = mean(m_rep, na.rm = TRUE), mass_loss_se    = se(m_rep),
                 n_reps       = length(p_rep))
}

# =============================================================================
# Run with_BB at the corrected colony size
# =============================================================================
wf <- load_windfarms(WINDFARM_SHP, target_crs = raster::crs(seamask),
                     include = names(WF_WITH_BB)[WF_WITH_BB])

Par_i <- Par
Par_i$Npairspercol       <- NPAIRS_NEW          # <- the only intentional change
Par_i$Nscalefactor       <- POP_FRACTION
Par_i$Pmedian            <- rep(CALIBRATED_PMEDIAN, N_REPLICATES)
Par_i$PreyType           <- "Uniform"
Par_i$OffalAccessFrac    <- 0
Par_i$OffalBiomass_g     <- 0
Par_i$OffalCell          <- NULL
Par_i$OffalEnergyDensity <- 9

modPar_i <- modPar; modPar_i$Nreplicates <- N_REPLICATES
ordPar_i <- ordPar; ordPar_i$include_ORDs <- wf$include_ORDs

message(sprintf("--- with_BB at Npairspercol = %d (%d adults simulated at %.0f%%) ---",
                NPAIRS_NEW, 2 * ceiling(NPAIRS_NEW * POP_FRACTION), 100 * POP_FRACTION))
message(sprintf("    reference: with_BB at Npairspercol = %d (%d adults simulated)",
                NPAIRS_OLD, 2 * ceiling(NPAIRS_OLD * POP_FRACTION)))

t0 <- Sys.time()
res <- seabord(
  Par = Par_i, modPar = modPar_i, ordPar = ordPar_i, switches = switches,
  seamask = seamask, spadat1 = spadat1, spadat2 = spadat2, spdat = spdat,
  BrdData = BrdData, FrgCompData = FrgCompData, fltdist_base = fltdist_base,
  FlightGridcorrection = FlightGridcorrection, ORDpoly = wf$ORDpoly,
  PreyMap = NULL, EnergyMap = NULL
)
mins <- round(as.numeric(Sys.time() - t0, units = "mins"), 1)

new <- get_metrics(res)
new$label <- "with_BB_6068"; new$Npairspercol <- NPAIRS_NEW; new$minutes <- mins

# Recompute the reference from 04's stored raw output with the SAME get_metrics,
# rather than reading its summary row, so the two sides are identically derived.
raw_2c  <- readRDS("outputs/bb_compensation_2c_raw.rds")
old_raw <- raw_2c[["with_BB"]]
old <- get_metrics(old_raw)
old$label <- "with_BB_2898"; old$Npairspercol <- NPAIRS_OLD; old$minutes <- NA_real_

cmp <- dplyr::bind_rows(old, new)
saveRDS(list(comparison = cmp, raw_new = res[setdiff(names(res), "BirdFlightMap")]),
        OUT_RDS)

# =============================================================================
# Verdict
# =============================================================================
d      <- new$productivity - old$productivity
se_d   <- sqrt(new$productivity_se^2 + old$productivity_se^2)
BB_DEFICIT <- 0.0700   # without_BB - with_BB, from 04

cat("\n=== COLONY SIZE CHECK ===\n")
print(as.data.frame(cmp), row.names = FALSE)
# The baselines were loaded from the legacy files and carry only the output_*
# tibbles, no thisRun -- so take the old competition constant from any config in
# the 2c sweep that ran in-process. It is a per-run constant, identical across
# them, and it does not depend on the offal treatment.
old_ppk <- raw_2c[["offalcell_200kg"]]$thisRun$popbirdsperkm2
cat(sprintf("\npopbirdsperkm2: %.4f -> %.4f (%+.1f%%)\n",
            old_ppk, res$thisRun$popbirdsperkm2,
            100 * (res$thisRun$popbirdsperkm2 / old_ppk - 1)))
z  <- d / se_d
p  <- 2 * stats::pnorm(-abs(z))
ci <- d + c(-1.96, 1.96) * se_d
material <- 0.1 * BB_DEFICIT          # what would count as contaminating the threshold
mde      <- 2 * se_d                  # smallest shift this run could have detected

cat(sprintf("\nproductivity %.4f -> %.4f\n", old$productivity, new$productivity))
cat(sprintf("  diff %+.4f  SE %.4f  z %.2f  p %.3f  95%% CI %+.4f to %+.4f\n",
            d, se_d, z, p, ci[1], ci[2]))
cat(sprintf("  that is %.0f%% of BB's own deficit (%.4f); 'material' bar is %.4f\n",
            100 * abs(d) / BB_DEFICIT, BB_DEFICIT, material))
cat(sprintf("  smallest shift this run could resolve (2xSE): %.4f\n", mde))

# --- The mechanistic bound -------------------------------------------------
# This is the part that does NOT depend on the simulation noise, and it is the
# reason the statistical result above is secondary.
#
# Competition enters intake through IRhalf only:
#   ComFactor = Birdsperkm2 / popbirdsperkm2      (functions-seabordday.R:173)
#   IRhalf    = IR_half_a * ComFactor^IR_half_b   (functions-seabordday.R:174)
# and IRhalf is the half-saturation constant of a Holling type II depletion
# solved in calc_foragecapture (functions-seabordbirds.R:562), so a LARGER
# IRhalf means SLOWER capture at the same prey density -- worse foraging.
#
# IR_half_b is ~0.02 for kittiwake, so IRhalf is almost perfectly insensitive to
# crowding: even a 100x change in ComFactor moves it under 10%. Whatever the
# noisy comparison above says, competition cannot deliver a productivity shift
# of the size that would matter here.
b  <- spdat$IR_half_b
a0 <- spdat$IR_half_a
cat(sprintf("\nMechanistic bound: IRhalf = %g * ComFactor^%g\n", a0, b))
for (cf in c(2, 10, 100))
  cat(sprintf("  ComFactor x%-4g -> IRhalf %.1f (%+.2f%%)\n", cf, a0 * cf^b, 100 * (cf^b - 1)))
cat("  (larger IRhalf = slower capture = worse foraging = lower productivity)\n")

# --- Verdict ----------------------------------------------------------------
# Judge the STATISTICS and the POWER separately. The old version collapsed them
# and reported a 'material shift' on a point estimate whose CI included zero.
sig <- (ci[1] > 0) || (ci[2] < 0)
if (!sig && mde <= material) {
  cat("\nVERDICT: no shift, and this run COULD have seen a material one.\n")
  cat("  Competition is inert at this colony size. Relabel the 2c dose axis; no re-run.\n")
} else if (!sig) {
  cat("\nVERDICT: INCONCLUSIVE ON ITS OWN -- consistent with zero, but underpowered.\n")
  cat(sprintf("  The CI includes zero, yet this run could only resolve shifts above %.4f,\n", mde))
  cat(sprintf("  which is coarser than the %.4f that would matter. The statistics cannot\n", material))
  cat("  settle it either way; fall back on the mechanistic bound printed above.\n")
  cat("  If that bound is small (it is, at IR_half_b ~ 0.02), relabelling is sound and\n")
  cat("  the residual shift should be carried as a caveat, not treated as a finding.\n")
  cat(sprintf("  To settle it statistically instead, raise N_REPLICATES until 2xSE < %.4f.\n", material))
} else if (abs(d) < material) {
  cat("\nVERDICT: real but small against the BB deficit. Relabelling is defensible;\n")
  cat("  report the shift as a caveat.\n")
} else {
  cat("\nVERDICT: real AND material -- and check it against the mechanistic bound above.\n")
  cat("  If the bound cannot produce a shift this size, suspect a confound (rep count,\n")
  cat("  seed window, population granularity) before concluding competition is the cause.\n")
  cat("  Only if the bound supports it should the 2c sweep be re-run at the new colony size.\n")
}
cat(sprintf("\nSaved: %s\n", OUT_RDS))
