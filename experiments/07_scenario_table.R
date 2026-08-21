################################################################################
## Experiment 07: full SeabORD output table by scenario
##
## Reads saved output only. Produces a table in the style of HiDef (2022)
## Table 3.2, but with our supplementary feeding scenarios as columns instead of
## SPAs. Rows are SeabORD output variables.
##
## Scenario labels follow the report:
##   A  three consented wind farms, no BBWF
##   B  three consented wind farms plus BBWF, no supplementary food
##   C-K  as B, with 50 to 4,000 kg/day of supplementary food
##
## All values are from the scenario ("scen") season, which is the season with
## wind farm effects applied. Distance and exposure rows are the difference
## between the scenario and baseline seasons within each run, which is how
## SeabORD reports them.
##
## Run 02 -> 03 first.
################################################################################

source("experiments/_setup_inputs.R")

RAW <- sprintf("outputs/bb_compensation_2c_%d_raw.rds", COLONY_PAIRS)
RES <- sprintf("outputs/bb_compensation_2c_%d_results.rds", COLONY_PAIRS)
SRV <- sprintf("outputs/comparable_survival_%d.csv", COLONY_PAIRS)
for (f in c(RAW, RES, SRV)) if (!file.exists(f)) stop("Missing ", f)

raw <- readRDS(RAW); res <- readRDS(RES)
srv <- readr::read_csv(SRV, show_col_types = FALSE)

# Scenario definitions: label -> config name, BBWF present, SF amount
SCEN <- tibble::tribble(
  ~id, ~config,             ~bbwf, ~sf,
  "A", "without_BB",        "No",     0,
  "B", "with_BB",           "Yes",    0,
  "C", "offalcell_50kg",    "Yes",   50,
  "D", "offalcell_100kg",   "Yes",  100,
  "E", "offalcell_150kg",   "Yes",  150,
  "F", "offalcell_200kg",   "Yes",  200,
  "G", "offalcell_300kg",   "Yes",  300,
  "H", "offalcell_500kg",   "Yes",  500,
  "I", "offalcell_1000kg",  "Yes", 1000,
  "J", "offalcell_2000kg",  "Yes", 2000,
  "K", "offalcell_4000kg",  "Yes", 4000
)
stopifnot(all(SCEN$config %in% names(raw)))

# --- Per-scenario extraction --------------------------------------------------
# Adult and chick tibbles are per timestep; take the final timestep of the
# scenario season, then average across replicates.
grab <- function(cfg) {
  last <- function(x, s = "scen") dplyr::bind_rows(x) %>%
    dplyr::filter(Season == s, !is.na(t)) %>%
    dplyr::group_by(Rep) %>% dplyr::filter(t == max(t)) %>% dplyr::ungroup()

  a  <- last(raw[[cfg]]$output_a0)
  c0 <- last(raw[[cfg]]$output_c0)
  f  <- dplyr::bind_rows(raw[[cfg]]$output_f0) %>% dplyr::filter(!is.na(Season))
  fs <- f %>% dplyr::filter(Season == "scen")
  fb <- f %>% dplyr::filter(Season == "base")
  r  <- res[res$label == cfg, ]

  list(
    n_adults      = mean(a$N_alive_ad + a$N_dead_ad),
    surv_bs       = 100 * mean(a$AdultsSurvivingBS),
    mass_t0       = mean(a$BM_adult_t0.mn),
    mass_end      = mean(a$BM_adult.mn),
    mass_end_sd   = sd(a$BM_adult.mn),
    mass_loss_pc  = 100 * mean((a$BM_adult_t0.mn - a$BM_adult.mn) / a$BM_adult_t0.mn),
    surv_common   = srv$surv_common[srv$config == cfg],
    trips         = mean(a$trips_n.mn),
    fly_h         = mean(a$flying_h.mn),
    forage_h      = mean(a$foraging_h.mn),
    nest_h        = mean(a$colony_h.mn),
    intake_g      = mean(a$forage_g.mn),
    km_base       = mean(fb$Tot_basickm.mn),
    km_extra      = mean(fs$Tot_extrakm.mn),
    km_extra_sd   = sd(fs$Tot_extrakm.mn),
    disp_birds    = mean(fs$DispGT0.sm),
    barr_birds    = mean(fs$BarrGT0.sm),
    days_none     = mean(fs$TotN_None.mn),
    trips_diff    = mean(fs$TotN_trips.mn) - mean(fb$TotN_trips.mn),
    n_nests       = mean(c0$N_alive_ch + c0$N_dead_ch),
    chicks_dead   = mean(c0$N_dead_ch),
    chicks_dead_sd= sd(c0$N_dead_ch),
    prod          = r$productivity,
    prod_se       = r$productivity_se,
    access_frac   = ifelse(is.na(r$access_frac), 0, r$access_frac)
  )
}
D <- lapply(SCEN$config, grab); names(D) <- SCEN$id

# --- Assemble ------------------------------------------------------------------
# Each row is one output variable; each column one scenario.
g   <- function(k) sapply(SCEN$id, function(i) D[[i]][[k]])
fmt <- function(k, dp = 3) sprintf(paste0("%.", dp, "f"), g(k))
msd <- function(k, ks, dp = 3)
  sprintf(paste0("%.", dp, "f (%.", dp, "f)"), g(k), g(ks))

rows <- list(
  c("Scenario definition", "BBWF present",                                  "",      SCEN$bbwf),
  c("Scenario definition", "Supplementary food provided",                   "kg/day", format(SCEN$sf, big.mark = ",")),
  c("Scenario definition", "Proportion of colony simulated",                "",      sprintf("%.1f", rep(POP_FRACTION, nrow(SCEN)))),
  c("Scenario definition", "Number of adult birds in simulation",           "",      sprintf("%.0f", g("n_adults"))),
  c("Scenario definition", "Number of nests in simulation",                 "",      sprintf("%.0f", g("n_nests"))),
  c("Supplementary food",  "Adults fed by supplementary food (whole colony)","",     sprintf("%.0f", round(g("access_frac") * N_ADULTS_REAL))),
  c("Supplementary food",  "Proportion of colony fed",                      "%",     sprintf("%.1f", 100 * g("access_frac"))),
  c("Adults",              "Adult survival at end of breeding season",      "%",     fmt("surv_bs", 1)),
  c("Adults",              "Initial adult body mass",                       "g",     fmt("mass_t0", 1)),
  c("Adults",              "Final adult body mass (SD)",                    "g",     msd("mass_end", "mass_end_sd", 2)),
  c("Adults",              "Adult mass loss over the season",               "%",     fmt("mass_loss_pc", 2)),
  c("Adults",              "Annual adult survival rate (common reference)",  "",      fmt("surv_common", 4)),
  c("Time and energy",     "Foraging trips per day",                        "",      fmt("trips", 2)),
  c("Time and energy",     "Time flying",                                   "h/day", fmt("fly_h", 2)),
  c("Time and energy",     "Time foraging",                                 "h/day", fmt("forage_h", 2)),
  c("Time and energy",     "Time at the nest",                              "h/day", fmt("nest_h", 2)),
  c("Time and energy",     "Food intake",                                   "g/day", fmt("intake_g", 1)),
  c("Wind farm exposure",  "Total distance flown without wind farm effect", "km",    fmt("km_base", 1)),
  c("Wind farm exposure",  "Extra distance flown due to wind farms (SD)",   "km",    msd("km_extra", "km_extra_sd", 2)),
  c("Wind farm exposure",  "Change in number of trips due to wind farms",   "",      fmt("trips_diff", 3)),
  c("Wind farm exposure",  "Adults displaced at least once",                "",      sprintf("%.0f", g("disp_birds"))),
  c("Wind farm exposure",  "Adults barriered at least once",                "",      sprintf("%.0f", g("barr_birds"))),
  c("Wind farm exposure",  "Days with no wind farm interaction (of 30)",    "",      fmt("days_none", 2)),
  c("Chicks",              "Chicks not surviving the season (SD)",          "",      msd("chicks_dead", "chicks_dead_sd", 1)),
  c("Chicks",              "Breeding success (proportion of nests fledging)","",     fmt("prod", 4)),
  c("Chicks",              "Standard error of breeding success",            "",      fmt("prod_se", 4))
)

out <- do.call(rbind, lapply(rows, function(r)
  as.data.frame(t(c(r[1], r[2], r[3], r[-(1:3)])), stringsAsFactors = FALSE)))
names(out) <- c("Group", "Output variable", "Unit", SCEN$id)
rownames(out) <- NULL

readr::write_csv(out, "outputs/scenario_table.csv")

cat(sprintf("\nScenarios: %s\n\n", paste(SCEN$id, collapse = " ")))
for (grp in unique(out$Group)) {
  cat(sprintf("--- %s ---\n", grp))
  sub <- out[out$Group == grp, -1]
  print(sub, row.names = FALSE, right = FALSE)
  cat("\n")
}
cat("Written: outputs/scenario_table.csv\n")
