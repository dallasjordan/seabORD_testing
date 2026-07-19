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
## N_REPLICATES reps and BOTH seasons. The baselines are loaded from the previous
## study, not re-run, so the cost is the offal-cell sweep: one config per entry in
## OFFAL_CELL_KG, ~70-80 min each at 10 reps. Prefer a background/overnight run.
## Results AND raw output are saved after every config and the script resumes from
## them, so an interruption loses at most one config.
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

# Per-bird foraging destinations: needed to verify the offal forcing actually
# fired and to analyse where birds fed. Adds rows but they are just integers.
switches$saveperbirddest <- TRUE

OFFAL_KJ_PER_G <- 9      # offal quality (kJ/g available to kittiwakes)
# This is applied to the WHOLE offal cell via EnergyMap, including the ~175 g of
# ordinary prey already there -- i.e. birds foraging the dump site are assumed to
# feed PREFERENTIALLY on offal rather than sampling the cell at random. That is
# the intended assumption: at any realistic deposit (>100 kg/day) offal is the
# overwhelming majority of what is available, so the blended density would be
# ~9 kJ/g anyway. It does flatter the two smallest swept doses, where the
# re-labelled baseline prey is a large share of the cell's energy (72% at
# 5 kg/day, 57% at 10 kg/day); treat those two points as upper bounds.

# --- Which analyses to run (toggle off the ones you don't need) ---------------
# RUN_BASELINES = FALSE is the normal setting: without_BB / with_BB were measured
# at 20 reps and are loaded back from the legacy output files, not re-run
# (~3 h each). Set TRUE only to re-measure them from scratch.
RUN_BASELINES  <- FALSE   # stage 1: without_BB vs with_BB  (the reportable BB impact)
RUN_SPATIAL    <- FALSE   # stage 2a: offal spread near colony, found randomly
RUN_PERBIRD    <- FALSE   # stage 2b: offal wherever birds forage (no travel effect)
RUN_OFFAL_CELL <- TRUE    # stage 2c: offal cell near colony, birds FLY to it  <- main scenario

# Re-measure every config instead of resuming the ones already in this sweep's
# output files. Normally FALSE -- an interrupted run should pick up where it left
# off. Set it TRUE if the model, the calibration or the deposit accounting has
# changed and the stored configs are no longer comparable with new ones.
#
# Assigned via get0() so that setting it in the console BEFORE sourcing survives:
#   FORCE_RERUN <- TRUE; source("experiments/04_bb_compensation_experiment.R")
# A plain `FORCE_RERUN <- FALSE` here would silently overwrite that.
FORCE_RERUN <- get0("FORCE_RERUN", ifnotfound = FALSE)

# Displacement (NatureScot: 30%, 2 km buffer) is set study-wide in
# _setup_inputs.R so every experiment uses the same assumption.

# Windfarm configurations (positional toggles resolved by load_windfarms)
WF_WITHOUT_BB <- c(INCAP = TRUE, SEAGREEN = TRUE, NEART = TRUE, BERWICK = FALSE)
WF_WITH_BB    <- c(INCAP = TRUE, SEAGREEN = TRUE, NEART = TRUE, BERWICK = TRUE)

# --- 2a. SPATIAL offal near the colony; birds find it RANDOMLY (via BrdData) ---
COLONY_RADIUS_M  <- 20000                            # disc radius around the Isle of May
SPATIAL_OFFAL_KG <- c(0, 2000, 5000, 10000, 20000)   # total biomass dumped (kg)

# --- 2b. Offal cell that a GUARANTEED fraction of birds always feed on ---------
# ACCESS_FRAC is a fixed ASSUMPTION here (not swept): this share of adults feeds
# on the offal every trip. We sweep the offal AMOUNT to find how much is needed.
ACCESS_FRAC_FIXED <- 0.47                            # 47% of adults always feed on offal
PERBIRD_OFFAL_KG  <- c(0, 0.2, 0.5, 1, 2, 5)         # offal available per accessing bird per trip (kg)
# NB: the intake half-saturation (IR_half_a) is 900 g, so this sweep deliberately
# spans below and above saturation -- past ~2-3 kg birds simply max out their
# intake and extra offal stops helping.

# --- 2c. OFFAL CELL near the colony that birds FLY TO (the main scenario) ------
# We dump offal in one cell near the Isle of May, and the share of adults that
# forage there on every trip (instead of their normal destination) is derived from
# how many birds the deposit can actually feed. Unlike 2b, they really travel to
# that cell, so this also captures:
#   - the shorter commute (flight is ~23% of daily energy at 4.7 h),
#   - the fact that they no longer route past Berwick Bank, so displacement
#     cannot touch them.
# That makes it the realistic "dump site" scenario -- but note the compensation
# it buys is offal energy PLUS avoided travel/displacement, not offal alone.
OFFAL_CELL_RADIUS_M    <- 20000  # search radius around the colony for the site

# --- HOW MANY BIRDS THE DEPOSIT CAN FEED --------------------------------------
# The access fraction is DERIVED from the deposit, not assumed. A dump of a given
# size supports a given number of kittiwake-days, and that is what sets how much
# of the colony can use it:
#
#   meals = deposit_g * (1 - OFFAL_OTHER_SPECIES_LOSS) / KITTIWAKE_DAILY_G
#   access fraction = meals / (adults in the REAL colony)     [capped at 1.0]
#
# The fraction is scale-invariant, which is why the real colony is the right
# denominator even though only POP_FRACTION of it is simulated. Dividing the
# scaled meals by the scaled population -- meals * POP_FRACTION / n_adults_sim --
# gives the identical number (0.2463 vs 0.2465 at 500 kg; the difference is the
# ceiling() in the simulated colony size). The direct form is used because it has
# no scaling step to get backwards.
OFFAL_OTHER_SPECIES_LOSS <- 0.20   # share taken by gulls/other scavengers
KITTIWAKE_DAILY_G        <- 280    # one kittiwake-day of offal (g)

offal_access_frac <- function(kg, n_adults_real) {
  if (kg <= 0) return(0)
  meals <- kg * 1000 * (1 - OFFAL_OTHER_SPECIES_LOSS) / KITTIWAKE_DAILY_G
  min(1, meals / n_adults_real)
}

# Offal dropped EVERY DAY (kg). kg = 0 means no patch at all -- birds forage
# exactly as in with_BB (the no-intervention reference).
OFFAL_CELL_KG          <- c(0, 50, 100, 150, 200, 300, 500, 1000, 2000, 4000)
# What each dose now resolves to (real colony 5796 adults, 580 simulated):
#
#     kg/day    meals   access frac   birds fed (of 580)   standing g/bird
#         50      143         0.025                   14               286
#        100      286         0.049                   29               276
#        150      429         0.074                   43               279
#        200      571         0.099                   57               281
#        300      857         0.148                   86               279
#        500     1429         0.246                  143               280
#       1000     2857         0.493                  286               280
#       2000     5714         0.986                  572               280
#       4000    11429         1.000 (capped)         580               552
#
# 100-300 kg brackets the crossing predicted below. Doses under ~25 kg/day were
# dropped: they feed 1-7 birds of 580, which cannot move productivity above its
# ~0.006 replicate SE, so they cost 75 min each to reproduce the with_BB point.
#
# Two consequences worth understanding before reading any result:
#  - Standing stock per bird is ~280 g at every dose BY CONSTRUCTION -- that is
#    what "one meal each" means. The dose no longer changes how well a fed bird
#    eats; it changes HOW MANY birds are fed. That is the intended mechanism.
#  - Above ~2030 kg/day the whole colony is already fed, the fraction caps at 1.0,
#    and surplus offal simply enriches the patch (280 -> 552 g at 4000 kg). Expect
#    productivity to plateau there.
#
# Do NOT reason about this from a single foraging bout. calc_strategy gives each
# bird n TRIPS per timestep (functions-seabordbirds.R: TForagegms = n *
# tcapt$captured_g[foragemins]) and each trip draws fresh from the full standing
# stock, so the per-trip requirement is Fg/n, not the daily total. The model picks
# the smallest n whose flying + foraging + colony time fits the day. Measured
# output, not arithmetic (mean over reps, scen season, end of season):
#
#     config             trips  forage_h  fly_h  colony_h  forage_g  ChicksPerNest
#     without_BB          1.97      8.44   3.83     21.68     209.3          0.543
#     with_BB             1.94      8.09   3.78     22.13     202.1          0.473
#     offalcell_0kg       1.94      8.13   3.81     22.07     202.6          0.481
#     offalcell_0.2kg     1.53      5.95   2.18     22.96     181.4          0.788
#     offalcell_5kg       1.53      5.10   2.18     23.37     178.1          0.791
#
# Read that carefully before interpreting any threshold:
#  - Flight time nearly HALVES (3.78 -> 2.18 h) the moment birds are diverted, at
#    ANY deposit > 0. That is the commute, and it does not scale with the dose.
#  - Foraging time DOES fall with the dose (8.09 -> 5.95 -> 5.10 h).
#  - Productivity does not care: 0.788 vs 0.791 across a 25x dose increase, even
#    though that bought 0.85 h/day less foraging. It had already saturated.
# Productivity tracks colony attendance (r = 0.93), not food gathered (r = -0.95,
# i.e. the better-fed birds gathered LESS because they stopped sooner).

# OFFAL_CELL_KG is the amount dropped EVERY TIMESTEP (a boat going out daily),
# not a season total. It is NOT divided across timesteps.
#
# HOW A DAILY DEPOSIT BECOMES A CELL VALUE -------------------------------------
# A cell's value is a STANDING STOCK, not a budget. seabORD offers it in full to
# every bird independently and resets it every timestep: prey depletes within one
# bird's foraging bout, but never between birds or between days
# (functions-seabordday.R reads Prey0 from PreyAvailable and never writes back).
#
# So a raw daily drop is not shared out by the model. Each accessing bird draws on
# the whole cell, and nothing caps the colony's total intake at what was dropped.
# Intake per bird is capped by the bird instead: it forages to its requirement and
# stops -- measured forage_g.mn is ~178-209 g/day per adult across every config
# run so far.
#
# The meal-based access fraction above is what restores the accounting. Each bird
# admitted to the patch is allocated one KITTIWAKE_DAILY_G meal, so the deposit
# constrains HOW MANY birds are fed rather than how richly. Since measured intake
# (~180 g) sits below the 280 g allocation, the birds collectively consume less
# than was dropped -- conservative, and the direction we want to err in.
#
# WHY THE EARLIER SWEEP WAS FLAT (0.788 / 0.792 / 0.797 / 0.792 / 0.791)
# It held the access fraction at 47% and varied only the amount, so every dose fed
# the same 273 birds. Offal is ADDITIVE (resolve_injection sets
# multiplier = 1 + offal/Pmedian), so even its smallest point put 375 g at 9 kJ/g
# against a 175 g at 6.52 kJ/g baseline -- already enough to relieve those birds
# completely. Raising the deposit 25x had nothing left to fix, while the 53% never
# admitted to the patch stayed exactly as they were. Tying access to the deposit
# is what turns that flat line into a real dose-response.
#
# Still bundled, and still not attributable to offal energy alone: a diverted bird
# also gets a shorter commute, immunity from displacement (it no longer routes
# past the windfarms), and a prey QUALITY upgrade (6.52 -> 9 kJ/g). All three
# arrive in full for any bird admitted to the patch, at any deposit. What the
# deposit now controls is how many birds receive that bundle.

colony_point <- COLONY_POINT   # from _setup_inputs.R

# =============================================================================
# Helpers
# =============================================================================
# Pull scen-season demographics, per replicate, then mean + Monte Carlo SE.
#
# SURVIVAL -- READ THIS BEFORE QUOTING ANY SURVIVAL NUMBER FROM THIS SCRIPT.
# output_y0$AdultsSurvivingYr is referenced to the SAME CONFIG's own base season
# (meanbm <- mean(YearBirds$base$BM_adult)), so it measures the ORD effect WITHIN
# a config. The offal cell overrides destinations in BOTH seasons
# (functions-seabordmain.R: FlightListA and FlightListB), so the intervention is
# present in base and scen alike and cancels out. The column below is therefore
# a DIAGNOSTIC ONLY -- it is named survival_native to make that impossible to
# forget, it is excluded from the threshold analysis and the plot, and the
# reportable annual survival comes from 05_comparable_survival.R, which
# re-references every config to one fixed mass.
#
# Do NOT use output_a0's N_alive_ad either: no adult dies during the 30-day
# chick-rearing season at realistic prey, so within-season survival is 1.0 by
# construction and carries no signal.
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

  # One value per replicate, so the spread ACROSS reps is available. Previously
  # only the mean survived, which left every threshold with no uncertainty.
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
                 survival_native    = mean(s_rep, na.rm = TRUE),  # DIAGNOSTIC ONLY
                 survival_native_se = se(s_rep),
                 n_reps             = length(p_rep))
}

# Run one full configuration and return its metrics + a label row.
run_config <- function(windfarms, label, mechanism, offal_amount,
                       PreyMap = NULL, EnergyMap = NULL,
                       offal_frac = 0, offal_biomass_g = 0, offal_cell = NULL,
                       standing_g = NA_real_, access_frac = NA_real_,
                       n_access = NA_integer_) {
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
  # What this dose actually resolved to inside the model: the per-bird standing
  # stock, and the derived share of the colony admitted to the patch. Recorded so
  # the dose axis can be read in the units that drive the response (birds fed),
  # not just the nominal daily deposit.
  m$standing_g <- standing_g; m$access_frac <- access_frac; m$n_access <- n_access
  m$minutes <- round(as.numeric(Sys.time() - t0, units = "mins"), 1)
  message(sprintf("   productivity=%.4f (SE %.4f, n=%d)  mass loss=%.3f  [survival_native=%.4f -- diagnostic, use 05]  (%.1f min)",
                  m$productivity, m$productivity_se, m$n_reps, m$mass_loss,
                  m$survival_native, m$minutes))

  # Keep the raw summary tibbles so ANY metric can be recomputed later without
  # re-simulating. (BirdFlightMap is dropped -- a 3.5M-cell raster per config
  # would bloat the file for no analytical gain.)
  # Keep EVERYTHING seabord returned except the BirdFlightMap, which is a
  # 3.5M-cell raster per config (~28 MB) and adds nothing we analyse. Storing the
  # rest means any metric can be recomputed later without re-simulating -- we
  # have already been forced into a full re-run once by discarding data.
  raw_store[[label]] <<- res[setdiff(names(res), "BirdFlightMap")]
  m
}

results   <- list()
raw_store <- list()

# --- Output files -------------------------------------------------------------
# This sweep writes to its OWN files. The earlier offal-cell sweep (which placed
# the RAW daily drop in the cell, before SHARE_DEPOSIT existed) stays untouched
# in the legacy files, which are only ever READ, and only for the baselines.
# Keeping the two sweeps in separate files is what removes the need to detect,
# purge and back up superseded rows.
RES_RDS <- "outputs/bb_compensation_2c_results.rds"   # written
RAW_RDS <- "outputs/bb_compensation_2c_raw.rds"       # written
LEGACY_RES <- "outputs/bb_compensation_results.rds"   # read-only
LEGACY_RAW <- "outputs/bb_compensation_raw.rds"       # read-only

save_progress <- function() {
  saveRDS(dplyr::bind_rows(results), RES_RDS)
  saveRDS(raw_store,                 RAW_RDS)
}

# --- Load the baselines -------------------------------------------------------
# without_BB / with_BB were measured at 20 reps and are not re-run. They are
# recomputed from the stored raw output so they carry the current get_metrics
# schema (per-replicate SEs), which the rows saved alongside them predate.
load_baseline <- function(lab, res_file, raw_file) {
  if (!file.exists(res_file) || !file.exists(raw_file)) return(NULL)
  prior <- readRDS(res_file)
  if (!lab %in% prior$label) return(NULL)
  raw <- readRDS(raw_file)
  if (is.null(raw[[lab]])) return(NULL)
  m <- get_metrics(raw[[lab]])
  m$label <- lab; m$mechanism <- "none"; m$offal_amount <- 0
  m$standing_g <- 0
  m$minutes <- prior$minutes[match(lab, prior$label)]
  raw_store[[lab]] <<- raw[[lab]]     # 05 needs without_BB's base season
  m
}

# --- Resume -------------------------------------------------------------------
# save_progress() runs after every config, so re-sourcing after an interruption
# picks up where it stopped. Everything in these files came from the current
# sweep, so anything present is valid and is simply skipped.
if (file.exists(RES_RDS) && file.exists(RAW_RDS)) {
  prior <- readRDS(RES_RDS)
  raw_store <- readRDS(RAW_RDS)
  for (i in seq_len(nrow(prior))) results[[prior$label[i]]] <- prior[i, ]
  message(sprintf("Resumed %d config(s) from this sweep: %s",
                  length(results), paste(names(results), collapse = ", ")))
}

# =============================================================================
# STAGE 1. Baselines -- the reportable BB impact on survival AND
# productivity. Run at high replication to detect adult survival changes.
# =============================================================================
if (RUN_BASELINES) {
  N_REPLICATES <- N_REPLICATES_BASELINE
  results[["without_BB"]] <- run_config(WF_WITHOUT_BB, "without_BB", "none", 0); save_progress()
  results[["with_BB"]]    <- run_config(WF_WITH_BB,    "with_BB",    "none", 0); save_progress()
} else {
  for (lab in c("without_BB", "with_BB")) {
    if (is.null(results[[lab]])) {
      results[[lab]] <- load_baseline(lab, LEGACY_RES, LEGACY_RAW)
      if (!is.null(results[[lab]]))
        message(sprintf("Loaded baseline %s from %s (n=%d reps).",
                        lab, LEGACY_RES, results[[lab]]$n_reps))
    }
  }
  save_progress()
}

# Both baselines are required by the analysis section. Fail loudly and early
# rather than 12 hours into a sweep.
if (is.null(results[["without_BB"]]) || is.null(results[["with_BB"]])) {
  stop("Baselines missing: could not load without_BB / with_BB from ", LEGACY_RES,
       " and ", LEGACY_RAW, ". Set RUN_BASELINES <- TRUE to re-measure them.")
}

target_prod <- results[["without_BB"]]$productivity
cat(sprintf("\nTARGET (without BB): productivity=%.4f (SE %.4f, n=%d)  mass loss=%.3f\n",
            target_prod, results[["without_BB"]]$productivity_se,
            results[["without_BB"]]$n_reps, results[["without_BB"]]$mass_loss))
cat(sprintf("WITH BB           : productivity=%.4f (SE %.4f, n=%d)  mass loss=%.3f\n",
            results[["with_BB"]]$productivity, results[["with_BB"]]$productivity_se,
            results[["with_BB"]]$n_reps, results[["with_BB"]]$mass_loss))
cat(sprintf("BB's cost         : productivity %+.4f  (the gap to close; SE of the difference ~%.4f)\n",
            results[["with_BB"]]$productivity - target_prod,
            sqrt(results[["without_BB"]]$productivity_se^2 +
                 results[["with_BB"]]$productivity_se^2)))
cat("Adult annual survival is NOT reported here -- run 05_comparable_survival.R.\n\n")

# =============================================================================
# STAGE 2a. Spatial offal near the colony -- sweep biomass
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
# One cell near the Isle of May holds offal, and the share of adults that forage
# there is DERIVED from how many kittiwake-days the deposit supports. They really
# travel there, so this captures the offal energy AND the shorter commute AND the
# fact that they no longer route past Berwick Bank (so displacement can't reach
# them) -- for however many birds the deposit can feed.
if (RUN_OFFAL_CELL) {
  n_adults_real <- 2 * Par$Npairspercol                      # whole colony
  n_adults      <- 2 * ceiling(Par$Npairspercol * POP_FRACTION)  # simulated subsample
  cat(sprintf("\n2c: sweeping the DAILY deposit; access fraction derived from it.\n"))
  cat(sprintf("    Colony %d adults (%d simulated at %.0f%%); %.0f%% of offal lost to other species;\n",
              n_adults_real, n_adults, 100 * POP_FRACTION, 100 * OFFAL_OTHER_SPECIES_LOSS))
  cat(sprintf("    one kittiwake-day = %d g.\n", KITTIWAKE_DAILY_G))
  done <- names(results)
  todo <- setdiff(paste0("offalcell_", OFFAL_CELL_KG, "kg"), done)
  if (length(todo) < length(OFFAL_CELL_KG)) {
    cat(sprintf("    Resuming: %d of %d dose(s) already measured; running %s.\n",
                length(OFFAL_CELL_KG) - length(todo), length(OFFAL_CELL_KG),
                if (length(todo)) paste(todo, collapse = ", ") else "nothing"))
  }
  for (kg in OFFAL_CELL_KG) {
    key <- paste0("offalcell_", kg, "kg")
    # Resume: everything in this sweep's own files is valid, so skip it.
    # FORCE_RERUN (declared with the other toggles at the top) overrides this.
    if (!isTRUE(FORCE_RERUN) && key %in% done) {
      message(sprintf("--- %s: already measured, skipping ---", key))
      next
    }
    if (kg == 0) {
      # No offal deposited -> no patch, nobody is sent anywhere. Birds forage
      # exactly as in with_BB. This is the "no intervention" reference point.
      results[[key]] <- run_config(WF_WITH_BB, key, "offalcell_kg", kg,
                                   standing_g = 0)
    } else {
      # How many birds this deposit can feed, and how much each one gets.
      frac     <- offal_access_frac(kg, n_adults_real)
      n_access <- round(n_adults * frac)
      if (n_access < 1) {
        message(sprintf("--- %s: feeds %.2f birds of %d simulated -- below one bird, skipping ---",
                        key, n_adults * frac, n_adults))
        next
      }
      # The share of the deposit belonging to the simulated subsample, divided
      # between the birds admitted to the patch. Offal is added ON TOP of the
      # cell's ordinary prey, so the patch is never poorer than a normal cell.
      # This is ~KITTIWAKE_DAILY_G by construction until the fraction caps at 1.
      standing_g <- kg * 1000 * (1 - OFFAL_OTHER_SPECIES_LOSS) * POP_FRACTION / n_access
      cat(sprintf("    %6.0f kg/day -> %5.1f%% access (%3d of %d birds), %6.0f g per bird (cell total %6.0f g)\n",
                  kg, 100 * frac, n_access, n_adults, standing_g,
                  standing_g + CALIBRATED_PMEDIAN))
      # Site the cell on the most-visited reachable sea cell within
      # OFFAL_CELL_RADIUS_M of the colony, holding that standing amount at 9 kJ/g.
      pt <- make_point_prey(
        seamask = seamask, center = COLONY_POINT,
        Pmedian_value = CALIBRATED_PMEDIAN, energy_prey_model = spdat$energy_prey,
        BrdData = BrdData, min_distance = 0, max_distance = OFFAL_CELL_RADIUS_M,
        target_mass_g = standing_g, offal_energy_density = OFFAL_KJ_PER_G)
      results[[key]] <- run_config(
        WF_WITH_BB, key, "offalcell_kg", kg,
        PreyMap = pt$PreyMap, EnergyMap = pt$EnergyMap,
        offal_frac = frac,            # derived from the deposit, not assumed
        offal_biomass_g = 0,          # 0 -> use the CELL's prey, not a blanket override
        offal_cell = pt$target_cell,  # birds are sent here every trip
        standing_g = standing_g, access_frac = frac, n_access = n_access)
      # Site is identical across kg, so write it once -- but on a resumed run the
      # first swept dose may be skipped, so key off the file instead of the dose.
      if (!file.exists("outputs/offal_cell_geometry.rds")) {
        saveRDS(pt, "outputs/offal_cell_geometry.rds")
      }
    }
    save_progress()
  }
}

# =============================================================================
# 4. Analysis: how much offal restores the target?
# =============================================================================
res_df <- dplyr::bind_rows(results)
readr::write_csv(res_df, "outputs/bb_compensation_2c_results.csv")
cat("\n=== All configurations ===\n"); print(as.data.frame(res_df), row.names = FALSE)

# Interpolate the offal amount at which each metric reaches the WITHOUT_BB target.
# Both mechanisms now titrate an AMOUNT (kg), under different access assumptions.
mech_desc <- c(
  spatial_kg       = "total kg dumped near the colony; birds find it randomly (BrdData)",
  perbird_offal_kg = sprintf("kg per bird per trip; %.0f%% feed on offal WHERE THEY ALREADY FORAGE (food effect only)",
                             100 * ACCESS_FRAC_FIXED),
  offalcell_kg     = "kg/day in a cell near the colony; the share of adults that FLY THERE is derived from the deposit (food + shorter trip + no displacement)"
)
mech_desc <- mech_desc[names(mech_desc) %in% unique(res_df$mechanism)]
# How much offal is needed to reach `target`?
#
# The previous version called approx(metric, amount, xout = target), i.e. it
# inverted the dose-response by treating the METRIC as the x axis. That is only
# valid if the metric is strictly monotone in the dose. It is not: the response
# saturates (see the OFFAL_CELL_KG note -- above ~500 kg/day the curve is flat to
# within a few minutes of foraging time) and it carries replicate noise, so the
# metric->amount mapping is not a function. approx() silently sorts on the metric
# and ties = mean collapses the plateau, which can return a precise-looking
# crossing that is pure noise. The plateau guard did not catch it either, because
# it only fired when max(metric) < target -- not the likelier case of a plateau
# straddling the target.
#
# This version instead walks the dose axis, enforces monotonicity with cummax,
# and reports the SMALLEST dose whose (running-best) metric reaches the target,
# interpolating linearly between the bracketing doses. It guards:
#  (a) no real deficit to close -> any fit would be noise;
#  (b) the response plateaus below the target -> more offal cannot help;
#  (c) the target is genuinely not reached within the swept range;
#  (d) the crossing is not resolved by the sweep's own noise -> flag it.
# `deficit` is the gap this metric actually has to close (target - with_BB).
# `se` (optional, per point) is used only to warn that a crossing is within noise.
need_amount <- function(metric, amount, target, deficit = NULL, noise = 0.01,
                        se = NULL) {
  ok <- is.finite(metric) & is.finite(amount)
  metric <- metric[ok]; amount <- amount[ok]
  se <- if (is.null(se)) rep(NA_real_, length(metric)) else se[ok]
  if (length(metric) < 2) return(list(value = NA_real_, note = "too few points"))

  # (a) no meaningful deficit -> nothing to restore
  if (!is.null(deficit) && abs(deficit) < noise) {
    return(list(value = NA_real_,
                note = sprintf("no deficit to close (BB effect %+.4f, within noise) - not binding", deficit)))
  }

  # Order by DOSE and take the running best, so a dip from replicate noise cannot
  # create a spurious second crossing.
  o <- order(amount); a <- amount[o]; m <- metric[o]; s <- se[o]
  mono <- cummax(m)

  hit <- which(mono >= target)[1]
  if (!is.na(hit)) {
    if (hit == 1L) {
      val  <- a[1]
      note <- sprintf("target already met at the lowest dose swept (%g kg) - add lower points to resolve it", a[1])
    } else {
      # Linear interpolation between the bracketing doses on the monotone hull.
      lo <- hit - 1L
      f  <- (target - mono[lo]) / (mono[hit] - mono[lo])
      val  <- a[lo] + f * (a[hit] - a[lo])
      note <- NA_character_
      # (d) is the crossing bigger than the noise on the points that bracket it?
      br_se <- suppressWarnings(max(s[c(lo, hit)], na.rm = TRUE))
      if (is.finite(br_se) && (mono[hit] - mono[lo]) < 2 * br_se) {
        note <- sprintf("crossing between %g and %g kg is within replicate noise (rise %.4f vs SE %.4f) - treat as unresolved",
                        a[lo], a[hit], mono[hit] - mono[lo], br_se)
      }
      # (e) the sweep spans orders of magnitude, so a crossing bracketed by a
      # wide dose gap is interpolated across a segment the sweep cannot resolve.
      # Linear interpolation over a 4x dose ratio is not a real answer.
      if (is.na(note) && a[lo] > 0 && (a[hit] / a[lo]) > 3) {
        note <- sprintf("crossing bracketed only by %g and %g kg (a %.0fx gap) - add points in between to localise it",
                        a[lo], a[hit], a[hit] / a[lo])
      }
    }
    return(list(value = val, note = note))
  }

  # (b) plateaued below target? Judge by the gain over the LAST STEP of the dose
  # axis, not by the spread over the last third: the sweep spans 5 kg to 4000 kg,
  # so a "last third" window covers a 16x dose range and a saturating curve still
  # varies across it by more than `noise` -- which made the old rule miss real
  # plateaus and wrongly advise extending the sweep.
  n <- length(m)
  last_gain <- m[n] - m[n - 1]
  if (last_gain < noise && max(m) < target) {
    return(list(value = NA_real_,
                note = sprintf("PLATEAUS at %.4f, short of target %.4f - more offal cannot close the gap (closes %.0f%% of it; last step %g->%g kg gained only %+.4f)",
                               max(m), target,
                               100 * (max(m) - m[1]) / (target - m[1]),
                               a[n - 1], a[n], last_gain)))
  }
  list(value = NA_real_,
       note = sprintf("target not reached within swept range, but still rising (last step %g->%g kg gained %+.4f) - extend the sweep",
                      a[n - 1], a[n], last_gain))
}

# BB's actual deficit. Productivity only: survival_native cancels across seasons
# for the offal configs and cannot be inverted here (see get_metrics).
deficit_prod <- target_prod - results[["with_BB"]]$productivity
cat(sprintf("\nBB deficit to close: productivity %+.4f\n", deficit_prod))

for (mech in names(mech_desc)) {
  d <- dplyr::filter(res_df, mechanism == mech) %>% dplyr::arrange(offal_amount)
  if (nrow(d) < 2) next
  p <- need_amount(d$productivity, d$offal_amount, target_prod,
                   deficit = deficit_prod, se = d$productivity_se)
  cat(sprintf("\n[%s]  (%s)\n", mech, mech_desc[[mech]]))
  cat("   dose-response (kg/day -> access% -> productivity +/- SE):\n")
  for (i in seq_len(nrow(d))) {
    cat(sprintf("     %8.0f  %5s%%  %.4f +/- %.4f  (n=%d)\n",
                d$offal_amount[i],
                if (is.na(d$access_frac[i])) "  -" else sprintf("%5.1f", 100*d$access_frac[i]),
                d$productivity[i], d$productivity_se[i], d$n_reps[i]))
  }
  if (is.na(p$value)) {
    cat(sprintf("   to restore productivity: %s\n", p$note))
    cat("   => NO SWEPT DEPOSIT OFFSETS BERWICK BANK.\n")
    cat("      Check the access column: if it reached ~100% and the target is still\n")
    cat("      out of reach, the dump site cannot close the gap at any tonnage.\n")
  } else {
    cat(sprintf("   => OFFAL NEEDED TO OFFSET BERWICK BANK: %.1f kg/day\n", p$value))
    if (!is.na(p$note)) cat(sprintf("      CAVEAT: %s\n", p$note))
    frac_at <- approx(d$offal_amount, d$access_frac, xout = p$value, rule = 2)$y
    if (is.finite(frac_at))
      cat(sprintf("      That deposit feeds ~%.0f%% of the colony.\n", 100 * frac_at))
    cat("      Bundled intervention: offal energy PLUS the shorter commute PLUS\n")
    cat("      escaping displacement, for every bird the deposit admits.\n")
  }
}
cat("\nAdult annual survival: run 05_comparable_survival.R.\n")

# =============================================================================
# 5. Dose-response plot
# =============================================================================
# Productivity only. survival_native is excluded on purpose -- it cancels across
# seasons for the offal configs, so plotting it would show a flat line that means
# nothing. The survival figure comes from 05_comparable_survival.R.
library(ggplot2)
plot_df <- dplyr::filter(res_df, mechanism %in% names(mech_desc))
p <- ggplot(plot_df, aes(offal_amount, productivity)) +
  geom_ribbon(aes(ymin = productivity - productivity_se,
                  ymax = productivity + productivity_se), alpha = 0.15) +
  geom_point() + geom_line() +
  geom_hline(yintercept = target_prod, lty = 2, colour = "forestgreen") +
  geom_hline(yintercept = results[["with_BB"]]$productivity, lty = 3, colour = "firebrick") +
  facet_wrap(~mechanism, scales = "free_x") +
  labs(title = "Offal needed to offset Berwick Bank (productivity)",
       subtitle = "green dashed = without-BB target | red dotted = with-BB | ribbon = +/-1 SE across reps | access fraction derived from the deposit",
       x = "Offal deposited per day (kg)", y = "Chicks per nest (proportion of nests fledging)")
ggsave("outputs/bb_compensation_2c_doseresponse.png", p, width = 10, height = 5, dpi = 150)

cat("\nDONE. Results: outputs/bb_compensation_2c_results.{rds,csv}; plot: outputs/bb_compensation_2c_doseresponse.png\n")
