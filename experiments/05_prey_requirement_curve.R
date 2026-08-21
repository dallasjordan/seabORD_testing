################################################################################
## Experiment 05: daily prey requirement per adult, by breeding success
##
## The seabORD equivalent of the SSE first-principles estimate. Reads the
## calibration sweep (experiment 01) only -- no re-simulation.
##
## Experiment 01 varies baseline prey density across a sweep and records, for
## each level, both the breeding success achieved and the food each adult
## actually gathered. Plotting one against the other gives the prey requirement
## implied by a target breeding success, measured rather than derived.
##
## Baseline season, no wind farms, so this is the undisturbed relationship.
################################################################################

source("experiments/_setup_inputs.R")
library(ggplot2)

CAL <- "outputs/calibration_output.rds"
if (!file.exists(CAL)) stop("Run 01_calibrate_pmedian.R first.")
cal <- readRDS(CAL)

# The SSE estimate being compared against (Abbatt, first principles).
abbatt <- data.frame(chicks = c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2),
                     g      = c(196, 205, 214, 223, 232, 241, 250, 259, 267))

# --- Pull breeding success and realised intake per prey level -----------------
last_step <- function(x) x %>% dplyr::filter(Season == "base", !is.na(t)) %>%
  dplyr::group_by(Rep) %>% dplyr::filter(t == max(t)) %>% dplyr::ungroup()

a  <- last_step(dplyr::bind_rows(cal$output_a0))
c0 <- last_step(dplyr::bind_rows(cal$output_c0))

d <- dplyr::left_join(
       a  %>% dplyr::select(Rep, Prey, intake = forage_g.mn,
                            fly = flying_h.mn, forage = foraging_h.mn,
                            nest = colony_h.mn),
       c0 %>% dplyr::select(Rep, ChicksPerNest), by = "Rep") %>%
     dplyr::arrange(ChicksPerNest)

cat(sprintf("\nCalibration sweep: %d prey levels, %g to %g g/cell, colony %d pairs\n",
            nrow(d), min(d$Prey), max(d$Prey), cal$Parameters$Npairspercol))
print(as.data.frame(d %>% dplyr::select(Prey, ChicksPerNest, intake, fly, forage, nest)),
      row.names = FALSE, digits = 4)

# --- Compare against the first-principles estimate -----------------------------
abbatt$seabord <- approx(d$ChicksPerNest, d$intake, xout = abbatt$chicks)$y
abbatt$diff    <- abbatt$seabord - abbatt$g
cat("\n=== seabORD vs first-principles estimate (g/adult/day) ===\n")
print(abbatt, row.names = FALSE, digits = 4)
ok <- !is.na(abbatt$seabord)
cat(sprintf("\nMean absolute difference where both are defined: %.1f g (%.1f%%)\n",
            mean(abs(abbatt$diff[ok])), 100 * mean(abs(abbatt$diff[ok] / abbatt$g[ok]))))

# The model cannot exceed one chick per nest: it holds one chick row per pair,
# so breeding success is a proportion. The sweep also plateaus below that.
ceiling_obs <- max(d$ChicksPerNest)
cat(sprintf("Highest breeding success reached in the sweep: %.3f (prey %g g/cell)\n",
            ceiling_obs, d$Prey[which.max(d$ChicksPerNest)]))

# --- Figure -------------------------------------------------------------------
# Points to label. Above about 185 g/cell the response plateaus and the points
# bunch within a couple of grams of each other, so only the endpoint is labelled
# there to avoid overlapping text.
lab <- d %>% dplyr::filter(Prey %in% c(150, 165, 175, 180, 200)) %>%
  # Label to the left for points near the right-hand edge, right otherwise.
  dplyr::mutate(lab_h = ifelse(intake > 232, 1.12, -0.18),
                lab_v = ifelse(intake > 232, -0.6, 1.3))

p <- ggplot() +
  annotate("rect", xmin = 1.0, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = "grey88", alpha = 0.55) +
  annotate("text", x = 1.005, y = 158, hjust = 0, size = 3.1, colour = "grey30",
           label = "not reachable: seabORD models\none chick per nest") +
  geom_line(data = abbatt, aes(chicks, g, colour = "First-principles estimate (SSE)"),
            linetype = "dashed", linewidth = 0.7) +
  geom_point(data = abbatt, aes(chicks, g, colour = "First-principles estimate (SSE)"),
             size = 1.9, shape = 17) +
  geom_line(data = d, aes(ChicksPerNest, intake, colour = "seabORD (calibration sweep)"),
            linewidth = 0.8) +
  geom_point(data = d, aes(ChicksPerNest, intake, colour = "seabORD (calibration sweep)"),
             size = 2.1) +
  geom_text(data = lab, aes(ChicksPerNest, intake, label = paste0(Prey, " g/cell")),
            size = 2.9, colour = "grey25", hjust = -0.12, vjust = 1.55) +
  scale_colour_manual(values = c("First-principles estimate (SSE)" = "#C62828",
                                 "seabORD (calibration sweep)"     = "#1565C0"),
                      name = NULL) +
  scale_x_continuous(breaks = seq(0.0, 1.2, 0.1), limits = c(0, 1.25)) +
  labs(title = "Daily prey intake per adult kittiwake, by breeding success achieved",
       subtitle = paste0("Isle of May, ", cal$Parameters$Npairspercol,
                         " pairs, baseline season with no wind farms. Labels give the ",
                         "baseline prey density driving each point."),
       x = "Breeding success (proportion of nests fledging a chick)",
       y = "Prey gathered per adult per day (g)") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        legend.position = c(0.02, 0.98), legend.justification = c(0, 1),
        legend.background = element_rect(fill = alpha("white", 0.85), colour = "grey80"),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(colour = "grey30", size = 9.5),
        plot.title.position = "plot")

ggsave("outputs/prey_requirement_curve_comparison.png", p, width = 9.5, height = 5.8, dpi = 300)

# --- Report figure: seabORD only ----------------------------------------------
# Same data without the external comparison, for use in the report. Titles are
# deliberately plain so they can be replaced with report styling.
cal_pt <- d[d$Prey == CALIBRATED_PMEDIAN, ]

# Axes are intake on x and breeding success on y, so the figure reads as a
# response curve: more food gathered, higher breeding success. Points are
# labelled with the baseline prey density that produced them.
p2 <- ggplot(d, aes(intake, ChicksPerNest)) +
  geom_line(linewidth = 0.8, colour = "grey15") +
  geom_point(size = 2.2, colour = "grey15") +
  geom_text(data = lab[lab$Prey != CALIBRATED_PMEDIAN, ],
            aes(label = paste0(Prey, " g/cell"), hjust = lab_h, vjust = lab_v),
            size = 2.9, colour = "grey35") +
  geom_point(data = cal_pt, size = 3.8, shape = 21, stroke = 1.2,
             colour = "#1565C0", fill = NA) +
  geom_text(data = cal_pt, aes(label = sprintf("calibrated baseline\n%g g/cell", Prey)),
            hjust = -0.14, vjust = 1.1, size = 3, colour = "#1565C0",
            lineheight = 0.95) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(
       x = "Prey gathered per adult per day (g)",
       y = "Percentage of nests fledging a chick") +
  # labs(title = "Daily prey intake per adult kittiwake, by breeding success achieved",
  #      subtitle = paste0("Isle of May, ", format(cal$Parameters$Npairspercol, big.mark = ","),
  #                        " pairs. Baseline season with no wind farms; labels give the ",
  #                        "baseline prey density driving each point.")
  #      ) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(colour = "grey30", size = 9.5),
        plot.title.position = "plot")

ggsave("outputs/prey_requirement_curve.png", p2, width = 9.5, height = 5.8, dpi = 300)
readr::write_csv(d, "outputs/prey_requirement_curve.csv")

cat("\nWritten: outputs/prey_requirement_curve.png             (report figure, seabORD only)\n")
cat("         outputs/prey_requirement_curve_comparison.png  (with the SSE estimate)\n")
cat("         outputs/prey_requirement_curve.csv\n")
