################################################################################
## Experiment 06: report figure for the offal-cell dose response
##
## Reads outputs/final_results.csv (written by 04) and redraws the dose-response
## figure at report quality. No re-simulation, so it is cheap to re-run after
## editing any of the presentation settings below.
##
## Run 02 -> 03 -> 04 first, so final_results.csv reflects the current sweep.
################################################################################

source("experiments/_setup_inputs.R")
library(ggplot2)

# =============================================================================
# Presentation settings
# =============================================================================
PLOT_TITLE    <- ""      # caption is supplied in the report; leave blank
PLOT_SUBTITLE <- ""
X_LAB         <- "SF (kg/day)"
Y_LAB         <- "Percentage of nests fledging a chick"

SHOW_CROSSING <- TRUE    # mark the SF amount at which the target is reached
CROSSING_ROUND <- 1      # round the annotated crossing to this many kg (1 = exact)
MAX_DEPOSIT   <- NA      # NA = plot all amounts; set e.g. 2000 to truncate
OUT_PNG       <- "outputs/doseresponse_figure.png"

# Reference-line labels and colours. Solid lines rather than dashed/dotted, so
# they are legible for readers who find fine line styles hard to distinguish.
LAB_TARGET <- "without BBWF, no SF"
LAB_WITHBB <- "with BBWF, no SF"
COL_TARGET <- "#1B7F3B"
COL_WITHBB <- "#C1272D"

# Replicate standard errors are between 0.002 and 0.006 at 20 replicates, which
# is under one per cent of the plotted y range and renders as a hairline. Error
# bars are drawn instead of a ribbon so the uncertainty is at least visible.
SHOW_ERRORBARS <- TRUE

# =============================================================================
# Data
# =============================================================================
RES <- "outputs/final_results.csv"
if (!file.exists(RES)) stop("Missing ", RES, ". Run 04_final_summary.R first.")
res <- readr::read_csv(RES, show_col_types = FALSE)

if (!identical(unique(res$colony_pairs), COLONY_PAIRS)) {
  stop(sprintf("final_results.csv was written for %s pairs but _setup_inputs.R says %d. Re-run 04.",
               paste(unique(res$colony_pairs), collapse = "/"), COLONY_PAIRS))
}

target <- res$productivity[res$label == "without_BB"]
withbb <- res$productivity[res$label == "with_BB"]

d <- res %>%
  dplyr::filter(mechanism == "offalcell_kg") %>%
  dplyr::arrange(kg_per_day)
if (!is.na(MAX_DEPOSIT)) d <- dplyr::filter(d, kg_per_day <= MAX_DEPOSIT)

# --- Crossing: smallest deposit whose running-best reaches the target ---------
mono <- cummax(d$productivity)
hit  <- which(mono >= target)[1]
cross_kg <- cross_pct <- NA_real_
if (!is.na(hit) && hit > 1) {
  lo <- hit - 1L
  f  <- (target - mono[lo]) / (mono[hit] - mono[lo])
  cross_kg  <- d$kg_per_day[lo]     + f * (d$kg_per_day[hit]     - d$kg_per_day[lo])
  cross_pct <- d$pct_colony_fed[lo] + f * (d$pct_colony_fed[hit] - d$pct_colony_fed[lo])
  cat(sprintf("Crossing: %.0f kg/day, feeding %.1f%% of the colony (~%.0f adults)\n",
              cross_kg, cross_pct, cross_pct / 100 * N_ADULTS_REAL))
  cat(sprintf("Bracketed by %g and %g kg/day; rise %.4f vs 2xSE %.4f -> %s\n",
              d$kg_per_day[lo], d$kg_per_day[hit], mono[hit] - mono[lo],
              2 * max(d$productivity_se[c(lo, hit)]),
              ifelse((mono[hit] - mono[lo]) < 2 * max(d$productivity_se[c(lo, hit)]),
                     "within replicate noise", "resolved")))
}
cat(sprintf("Target %.4f | with Berwick Bank %.4f | deficit %+.4f (%+.0f chicks)\n",
            target, withbb, withbb - target, (withbb - target) * COLONY_PAIRS))

# =============================================================================
# Figure
# =============================================================================
xmax  <- max(d$kg_per_day)
lab_x <- xmax * 0.985

p <- ggplot(d, aes(kg_per_day, productivity)) +
  geom_hline(yintercept = target, colour = COL_TARGET, linewidth = 0.9) +
  geom_hline(yintercept = withbb, colour = COL_WITHBB, linewidth = 0.9)

# Replicate standard errors are 0.002 to 0.006 at 20 replicates, under one per
# cent of the plotted y range, so a ribbon renders as a hairline. Error bars at
# least show as visible ticks.
if (SHOW_ERRORBARS)
  p <- p + geom_errorbar(aes(ymin = productivity - productivity_se,
                             ymax = productivity + productivity_se),
                         width = xmax * 0.010, linewidth = 0.4, colour = "grey35")

p <- p +
  geom_line(linewidth = 0.7, colour = "grey15") +
  geom_point(size = 2.1, colour = "grey15") +
  annotate("text", x = lab_x, y = target, vjust = -0.9, hjust = 1,
           label = LAB_TARGET, colour = COL_TARGET, size = 3.6, fontface = "bold") +
  annotate("text", x = lab_x, y = withbb, vjust = 1.8, hjust = 1,
           label = LAB_WITHBB, colour = COL_WITHBB, size = 3.6, fontface = "bold") +
  scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 7),
                     expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  #labs(title = PLOT_TITLE, subtitle = PLOT_SUBTITLE) +
  labs(x = X_LAB, y = Y_LAB) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 12.5),
        plot.subtitle = element_text(colour = "grey30", size = 10),
        plot.title.position = "plot")

if (SHOW_CROSSING && is.finite(cross_kg)) {
  p <- p +
    annotate("segment", x = cross_kg, xend = cross_kg, y = -Inf, yend = target,
             linetype = "dotdash", colour = "grey40", linewidth = 0.4) +
    annotate("point", x = cross_kg, y = target, size = 2.6, shape = 21,
             fill = "white", colour = "grey20", stroke = 0.9) +
    annotate("text", x = cross_kg + 0.012 * xmax, y = target, hjust = 0, vjust = 1.9,
             size = 3.4, colour = "grey25",
             label = sprintf("%.0f kg/day",
                             round(cross_kg / CROSSING_ROUND) * CROSSING_ROUND))
}

ggsave(OUT_PNG, p, width = 9.5, height = 5.5, dpi = 300)
cat(sprintf("\nWritten: %s\n", OUT_PNG))
