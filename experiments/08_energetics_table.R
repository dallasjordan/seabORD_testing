################################################################################
## Experiment 08: displacement exposure and energetics by scenario
##
## Reads saved output only. Produces the table supporting the section on how
## supplementary feeding delivers its benefit, separating:
##   1. avoided wind farm exposure  (extra distance flown because of the farms)
##   2. shorter commute             (baseline-season distance flown)
##   3. better food                 (time spent foraging, food gathered)
##
## The separation works because SeabORD runs each configuration twice: a
## baseline season with no wind farm effect and a scenario season with it
## applied. The supplementary feeding site is present in BOTH seasons, so
##   baseline-season distance  = commute, with SF, without wind farm effects
##   Tot_extrakm               = the wind farm detour alone
##
## Run 02 first.
################################################################################

source("experiments/_setup_inputs.R")

RAW <- sprintf("outputs/bb_compensation_2c_%d_raw.rds", COLONY_PAIRS)
if (!file.exists(RAW)) stop("Missing ", RAW)
raw <- readRDS(RAW)

SCEN <- tibble::tribble(
  ~id, ~config,            ~sf,
  "A", "without_BB",          0,
  "B", "with_BB",             0,
  "C", "offalcell_50kg",     50,
  "D", "offalcell_100kg",   100,
  "E", "offalcell_150kg",   150,
  "F", "offalcell_200kg",   200,
  "G", "offalcell_300kg",   300,
  "H", "offalcell_500kg",   500,
  "I", "offalcell_1000kg", 1000,
  "J", "offalcell_2000kg", 2000,
  "K", "offalcell_4000kg", 4000
)

grab <- function(cfg) {
  last <- function(s) dplyr::bind_rows(raw[[cfg]]$output_a0) %>%
    dplyr::filter(Season == s, !is.na(t)) %>%
    dplyr::group_by(Rep) %>% dplyr::filter(t == max(t)) %>% dplyr::ungroup()
  a  <- last("scen")
  f  <- dplyr::bind_rows(raw[[cfg]]$output_f0) %>% dplyr::filter(!is.na(Season))
  dd <- raw[[cfg]]$output_dest

  # Share of bird-days displaced, and for birds displaced at least once, how
  # many of the 30 days they were displaced on.
  s  <- dd[dd$Season == "scen", ]
  pb <- s %>% dplyr::group_by(Rep, BirdID) %>%
    dplyr::summarise(nd = sum(Displaced), .groups = "drop")

  list(
    pct_disp  = 100 * mean(s$Displaced),
    days_disp = if (any(pb$nd > 0)) mean(pb$nd[pb$nd > 0]) else 0,
    n_disp   = mean(f$DispGT0.sm[f$Season == "scen"]),
    km_base  = mean(f$Tot_basickm.mn[f$Season == "base"]),
    km_extra = mean(f$Tot_extrakm.mn[f$Season == "scen"]),
    fly      = mean(a$flying_h.mn),
    forage   = mean(a$foraging_h.mn),
    nest     = mean(a$colony_h.mn),
    sea      = mean(a$at_sea_h.mn),
    intake   = mean(a$forage_g.mn)
  )
}
D <- lapply(SCEN$config, grab); names(D) <- SCEN$id
g <- function(k) sapply(SCEN$id, function(i) D[[i]][[k]])

pct_wf <- 100 * g("km_extra") / (g("km_base") + g("km_extra"))

rows <- list(
  # output_dest holds one row per bird per timestep, so this is the share of
  # bird-days on which a bird was displaced, not the share of foraging trips.
  # A bird makes roughly two trips to a single destination each day, so the
  # proportion is the same either way but the underlying count is not.
  c("Wind farm exposure", "Bird-days on which the bird was displaced",   "%",        sprintf("%.2f", g("pct_disp"))),
  c("Wind farm exposure", "Adults displaced at least once (of 1,214)",   "",         sprintf("%.0f", g("n_disp"))),
  c("Wind farm exposure", "Days displaced, for birds ever displaced",    "of 30",    sprintf("%.1f", g("days_disp"))),
  c("Flight distance",    "Commute, no wind farm effect",                "km/season", sprintf("%.0f", g("km_base"))),
  c("Flight distance",    "Extra distance due to wind farms",            "km/season", sprintf("%.1f", g("km_extra"))),
  c("Flight distance",    "Wind farm detour as share of total flight",   "%",        sprintf("%.1f", pct_wf)),
  c("Time budget",        "Flying",                                      "h/day",    sprintf("%.2f", g("fly"))),
  c("Time budget",        "Foraging",                                    "h/day",    sprintf("%.2f", g("forage"))),
  c("Time budget",        "At the nest",                                 "h/day",    sprintf("%.2f", g("nest"))),
  c("Time budget",        "Resting at sea",                              "h/day",    sprintf("%.2f", g("sea"))),
  c("Food",               "Food gathered",                               "g/day",    sprintf("%.1f", g("intake")))
)

out <- do.call(rbind, lapply(rows, function(r)
  as.data.frame(t(c(r[1], r[2], r[3], r[-(1:3)])), stringsAsFactors = FALSE)))
names(out) <- c("Group", "Output variable", "Unit", SCEN$id)
rownames(out) <- NULL
readr::write_csv(out, "outputs/energetics_table.csv")

for (grp in unique(out$Group)) {
  cat(sprintf("--- %s ---\n", grp))
  print(out[out$Group == grp, -1], row.names = FALSE, right = FALSE)
  cat("\n")
}

# --- Decomposition of the flight saving ---------------------------------------
B <- D[["B"]]
cat("=== Flight distance saved relative to Scenario B ===\n")
cat(sprintf("%-5s %14s %14s %16s\n", "Scen", "commute (km)", "WF detour (km)", "commute share"))
for (i in SCEN$id[-(1:2)]) {
  dc <- B$km_base  - D[[i]]$km_base
  dw <- B$km_extra - D[[i]]$km_extra
  cat(sprintf("%-5s %14.0f %14.1f %15.0f%%\n", i, dc, dw, 100 * dc / (dc + dw)))
}
cat(sprintf("\nAt Scenario B the wind farm detour is %.1f%% of total distance flown.\n",
            100 * B$km_extra / (B$km_base + B$km_extra)))
cat("\nWritten: outputs/energetics_table.csv\n")
