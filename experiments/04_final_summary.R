################################################################################
## Experiment 04: reporting layer
##
## Reads saved output only -- no re-simulation. Safe to re-run at any time.
## Requires 02 and 03 to have run at the current COLONY_PAIRS.
##
## Answers the two study questions:
##   Q1. What does each offal deposit buy?  -> chicks gained and adult survival
##       change, per dose, against the with-BB world.
##   Q2. How much offal offsets Berwick Bank? -> the deposit at which each metric
##       returns to the without-BB level.
##
## Writes: outputs/final_results.csv, outputs/final_doseresponse.png
################################################################################

source("experiments/_setup_inputs.R")
library(ggplot2)

# A deposit to report in detail (kg/day). Set NA to skip that section.
REPORT_KG <- 2000

# Must match 02, or the access fractions below will not describe the runs.
OFFAL_OTHER_SPECIES_LOSS <- 0.20
KITTIWAKE_DAILY_G        <- 280

kg_to_frac <- function(kg) pmin(1, kg * 1000 * (1 - OFFAL_OTHER_SPECIES_LOSS) /
                                   KITTIWAKE_DAILY_G / N_ADULTS_REAL)

# =============================================================================
# Load
# =============================================================================
RES <- sprintf("outputs/bb_compensation_2c_%d_results.rds", COLONY_PAIRS)
SRV <- sprintf("outputs/comparable_survival_%d.csv", COLONY_PAIRS)
for (f in c(RES, SRV)) if (!file.exists(f))
  stop("Missing ", f, ". Run 02 then 03 at COLONY_PAIRS = ", COLONY_PAIRS, ".")

res  <- readRDS(RES)
surv <- readr::read_csv(SRV, show_col_types = FALSE)

stopifnot(all(c("without_BB", "with_BB") %in% res$label))
gp <- function(l) res$productivity[res$label == l]
gs <- function(l, col) surv[[col]][surv$config == l]

target_prod <- gp("without_BB"); withbb_prod <- gp("with_BB")
target_surv <- gs("without_BB", "surv_common"); withbb_surv <- gs("with_BB", "surv_common")
target_band <- gs("without_BB", "surv_band");   withbb_band <- gs("with_BB", "surv_band")

# Chicks, not proportions. ChicksPerNest is the share of nests fledging a chick
# and seabORD models one chick per nest, so proportion x pairs = chicks.
# Real kittiwakes fledge 1-2, so these counts are a FLOOR (see NOTES.md).
all_cfg <- res %>%
  dplyr::left_join(surv %>% dplyr::select(label = config, surv_common, surv_band,
                                          surv_native = native_surv),
                   by = "label") %>%
  dplyr::mutate(
    colony_pairs         = COLONY_PAIRS,
    pct_colony_fed       = 100 * access_frac,
    birds_fed            = ifelse(is.na(access_frac), NA_real_,
                                  round(access_frac * N_ADULTS_REAL)),
    chicks_fledged       = productivity * COLONY_PAIRS,
    chicks_vs_with_BB    = (productivity - withbb_prod) * COLONY_PAIRS,
    chicks_vs_without_BB = (productivity - target_prod) * COLONY_PAIRS,
    surv_vs_with_BB      = surv_common - withbb_surv,
    surv_vs_without_BB   = surv_common - target_surv)

d <- all_cfg %>% dplyr::filter(mechanism == "offalcell_kg") %>%
  dplyr::arrange(offal_amount)

cat(sprintf("\n%s\n colony %d pairs (%d adults) | displacement %.0f%% | barrier %.0f%% | %d reps\n%s\n",
            strrep("=", 78), COLONY_PAIRS, N_ADULTS_REAL,
            100 * PROB_DISPLACEMENT, 100 * PROB_BARRIER,
            max(res$n_reps, na.rm = TRUE), strrep("=", 78)))

cat("\nBASELINES\n")
cat(sprintf("  without BB : nest success %.4f | %6.0f chicks | adult survival %.4f\n",
            target_prod, target_prod * COLONY_PAIRS, target_surv))
cat(sprintf("  with BB    : nest success %.4f | %6.0f chicks | adult survival %.4f\n",
            withbb_prod, withbb_prod * COLONY_PAIRS, withbb_surv))
cat(sprintf("  BB's cost  : %+.4f nest success = %+.0f chicks | survival %+.4f\n",
            withbb_prod - target_prod, (withbb_prod - target_prod) * COLONY_PAIRS,
            withbb_surv - target_surv))

# =============================================================================
# Q1. What does each deposit buy?
# =============================================================================
cat("\n\nQ1. WHAT EACH DEPOSIT BUYS (vs with BB, no offal)\n\n")
cat(sprintf("%9s %7s %10s %13s %9s %11s %11s\n",
            "kg/day", "% fed", "birds fed", "nest success", "chicks",
            "chicks +", "survival +"))
for (i in seq_len(nrow(d)))
  cat(sprintf("%9.0f %6.1f%% %10.0f %13.4f %9.0f %+11.0f %+11.4f\n",
              d$offal_amount[i], d$pct_colony_fed[i], d$birds_fed[i],
              d$productivity[i], d$chicks_fledged[i],
              d$chicks_vs_with_BB[i], d$surv_vs_with_BB[i]))

# =============================================================================
# Q2. How much offal offsets Berwick Bank?
# =============================================================================
# Walks the dose axis, enforces monotonicity with cummax, and reports the
# smallest dose whose running-best metric reaches the target, interpolating
# between the bracketing doses. Guards:
#  (a) no real deficit to close -> any fit would be noise
#  (b) the response plateaus below the target
#  (c) the target is not reached within the swept range
#  (d) the crossing is smaller than the sweep's own replicate noise
#  (e) the crossing is bracketed by doses too far apart to localise
#
# Do not invert with approx(metric, amount, xout = target): that treats the
# metric as the x axis, which is only valid if it is strictly monotone in the
# dose. It saturates and carries noise, so a plateau straddling the target
# returns a precise-looking crossing that is pure noise.
need_amount <- function(metric, amount, target, deficit = NULL, noise = 0.01,
                        se = NULL) {
  ok <- is.finite(metric) & is.finite(amount)
  metric <- metric[ok]; amount <- amount[ok]
  se <- if (is.null(se)) rep(NA_real_, length(metric)) else se[ok]
  if (length(metric) < 2) return(list(value = NA_real_, note = "too few points"))

  if (!is.null(deficit) && abs(deficit) < noise) {
    return(list(value = NA_real_,
                note = sprintf("no deficit to close (BB effect %+.4f, within noise %.3f) - not binding",
                               deficit, noise)))
  }

  o <- order(amount); a <- amount[o]; m <- metric[o]; s <- se[o]
  mono <- cummax(m)
  hit <- which(mono >= target)[1]

  if (!is.na(hit)) {
    if (hit == 1L)
      return(list(value = a[1],
                  note = sprintf("target already met at the lowest dose swept (%g kg) - add lower points",
                                 a[1])))
    lo <- hit - 1L
    f  <- (target - mono[lo]) / (mono[hit] - mono[lo])
    val  <- a[lo] + f * (a[hit] - a[lo])
    note <- NA_character_
    br_se <- suppressWarnings(max(s[c(lo, hit)], na.rm = TRUE))
    if (is.finite(br_se) && (mono[hit] - mono[lo]) < 2 * br_se)
      note <- sprintf("crossing between %g and %g kg is within replicate noise (rise %.4f vs SE %.4f) - unresolved",
                      a[lo], a[hit], mono[hit] - mono[lo], br_se)
    if (is.na(note) && a[lo] > 0 && (a[hit] / a[lo]) > 3)
      note <- sprintf("crossing bracketed only by %g and %g kg (a %.0fx gap) - add points in between",
                      a[lo], a[hit], a[hit] / a[lo])
    return(list(value = val, note = note))
  }

  # Plateaued below target? Judge by the gain over the LAST STEP of the dose
  # axis; a wider window spans too large a dose range on a saturating curve.
  n <- length(m)
  last_gain <- m[n] - m[n - 1]
  if (last_gain < noise && max(m) < target)
    return(list(value = NA_real_,
                note = sprintf("PLATEAUS at %.4f, short of %.4f - more offal cannot close the gap (closes %.0f%%; last step %g->%g kg gained %+.4f)",
                               max(m), target, 100 * (max(m) - m[1]) / (target - m[1]),
                               a[n - 1], a[n], last_gain)))
  list(value = NA_real_,
       note = sprintf("target not reached within swept range but still rising (last step %g->%g kg gained %+.4f) - extend the sweep",
                      a[n - 1], a[n], last_gain))
}

report_threshold <- function(nm, metric, se, target, deficit, noise) {
  cat(sprintf("\n  %s\n", nm))
  cat(sprintf("    BB's deficit to close: %+.4f\n", deficit))
  p <- need_amount(metric, d$offal_amount, target, deficit = deficit,
                   noise = noise, se = se)
  if (is.na(p$value)) {
    cat(sprintf("    => %s\n", p$note))
  } else {
    cat(sprintf("    => %.0f kg/day offsets Berwick Bank\n", p$value))
    cat(sprintf("       feeds %.0f%% of the colony (~%.0f adults)\n",
                100 * kg_to_frac(p$value), kg_to_frac(p$value) * N_ADULTS_REAL))
    if (!is.na(p$note)) cat(sprintf("       CAVEAT: %s\n", p$note))
    # Without a per-point SE the replicate-noise guard cannot fire, so a small
    # deficit can produce a confident-looking threshold that is really just
    # "almost any deposit clears it". Say so rather than let it read as precise.
    if (is.null(se))
      cat(sprintf("       CAVEAT: no replicate SE for this metric, so the noise guard\n%s",
                  "               did not run. Judge it against the deficit above --\n               a small deficit is cleared by almost any deposit.\n"))
  }
  invisible(p)
}

cat("\n\nQ2. OFFAL NEEDED TO OFFSET BERWICK BANK\n")
# Noise floors differ by metric: breeding success is a proportion over ~600
# nests and carries real replicate scatter; survival is a smooth function of
# mean mass and moves on a much smaller scale, so it needs a tighter floor or a
# real-but-small deficit would be dismissed as noise.
th_prod <- report_threshold("Breeding success (nests fledging a chick)",
                            d$productivity, d$productivity_se,
                            target_prod, target_prod - withbb_prod, noise = 0.01)
th_surv <- report_threshold("Adult annual survival (common reference)",
                            d$surv_common, NULL,
                            target_surv, target_surv - withbb_surv, noise = 0.002)

cat("\n  Bundled: any diverted bird gets offal energy PLUS a shorter commute PLUS\n")
cat("  immunity from displacement. Report as a dump-site intervention.\n")

# =============================================================================
# A nominated deposit in detail
# =============================================================================
if (is.finite(REPORT_KG) &&
    REPORT_KG >= min(d$offal_amount) && REPORT_KG <= max(d$offal_amount)) {
  interp <- function(y) approx(d$offal_amount, y, xout = REPORT_KG)$y
  p_at  <- interp(d$productivity)
  s_at  <- interp(d$surv_common)
  sb_at <- interp(d$surv_band)
  near  <- which.min(abs(d$offal_amount - REPORT_KG))

  cat(sprintf("\n\nAT %g kg/day (%.1f t/day)\n", REPORT_KG, REPORT_KG / 1000))
  cat(sprintf("  feeds %.0f of %d adults (%.1f%%); nearest measured dose %g kg/day\n",
              kg_to_frac(REPORT_KG) * N_ADULTS_REAL, N_ADULTS_REAL,
              100 * kg_to_frac(REPORT_KG), d$offal_amount[near]))
  contrast <- function(nm, bp, bs, bb) {
    cat(sprintf("  vs %s:\n", nm))
    cat(sprintf("    nest success   %.4f -> %.4f (%+.4f, %+.1f%%)\n",
                bp, p_at, p_at - bp, 100 * (p_at - bp) / bp))
    cat(sprintf("    chicks         %.0f -> %.0f (%+.0f per season)\n",
                bp * COLONY_PAIRS, p_at * COLONY_PAIRS, (p_at - bp) * COLONY_PAIRS))
    cat(sprintf("    adult survival %.4f -> %.4f (%+.4f | band %+.4f)\n",
                bs, s_at, s_at - bs, sb_at - bb))
  }
  contrast("with BB, no offal", withbb_prod, withbb_surv, withbb_band)
  contrast("without BB",        target_prod, target_surv, target_band)
}

# =============================================================================
# Health warnings
# =============================================================================
cat("\n\nREAD BEFORE QUOTING\n")
cat("* SCALE. 'nest success' is the PROPORTION of nests fledging a chick, not\n")
cat("  chicks per nest. Published Isle of May productivity (0.68 in 2025, 0.70\n")
cat("  average) is a COUNT and can exceed 1, since real nests fledge 1-2 chicks:\n")
cat("     chicks per nest = proportion fledging x mean brood of successful nests\n")
cat("  Do not compare the two directly. The chick counts here are a FLOOR.\n")
cat("* BUNDLED. Diverted birds also get a shorter commute and stop routing past\n")
cat("  Berwick Bank. This is a dump-site intervention, not 'offal energy'.\n")
cat("* CROWDING IS UNPENALISED. Fed birds all forage in one 1 km cell and the\n")
cat("  competition term is near-inert. Treat as an upper bound.\n")
cat("* Survival is computed from MEAN mass -- comparative, not an absolute rate.\n")
cat(sprintf("* PROB_BARRIER = %.1f scales BB's impact and therefore every number here.\n",
            PROB_BARRIER))

# =============================================================================
# Outputs
# =============================================================================
# surv_native is seabORD's own AdultsSurvivingYr, carried through for reference.
# It is referenced to each config's own base season, so it cancels for the offal
# configs -- diagnostic only, never the reportable figure.
out <- all_cfg %>%
  dplyr::select(label, mechanism, kg_per_day = offal_amount,
                colony_pairs, pct_colony_fed, birds_fed,
                productivity, productivity_se,
                chicks_fledged, chicks_vs_with_BB, chicks_vs_without_BB,
                surv_common, surv_band, surv_vs_with_BB, surv_vs_without_BB,
                surv_native, mass_loss, mass_loss_se, n_reps)
readr::write_csv(out, "outputs/final_results.csv")

p <- ggplot(d, aes(offal_amount, productivity)) +
  geom_hline(yintercept = target_prod, linetype = "dashed",
             colour = "#2E7D32", linewidth = 0.5) +
  geom_hline(yintercept = withbb_prod, linetype = "dotted",
             colour = "#C62828", linewidth = 0.5) +
  geom_ribbon(aes(ymin = productivity - productivity_se,
                  ymax = productivity + productivity_se),
              fill = "grey30", alpha = 0.18) +
  geom_line(linewidth = 0.7, colour = "grey15") +
  geom_point(size = 2.1, colour = "grey15") +
  annotate("text", x = max(d$offal_amount) * 0.985, y = target_prod,
           vjust = -0.8, hjust = 1, label = "Without Berwick Bank",
           colour = "#2E7D32", size = 3.3) +
  annotate("text", x = max(d$offal_amount) * 0.985, y = withbb_prod,
           vjust = 1.7, hjust = 1, label = "With Berwick Bank, no offal",
           colour = "#C62828", size = 3.3) +
  scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 7),
                     expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Offal required to offset Berwick Bank impacts on kittiwake breeding success",
       subtitle = "Shaded band = 1 SE across replicates",
       x = "Offal deposited per day (kg)", y = "Nests fledging a chick") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 12.5),
        plot.subtitle = element_text(colour = "grey30", size = 10),
        plot.title.position = "plot")

ggsave("outputs/final_doseresponse.png", p, width = 9.5, height = 5.5, dpi = 300)

cat("\nWritten:\n")
cat("  outputs/final_results.csv       (all configs, chicks and survival)\n")
cat("  outputs/final_doseresponse.png  (report figure)\n")
