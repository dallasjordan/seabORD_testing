################################################################################
## Helpers for building "transect-style" spatial prey enrichment scenarios.
##
## Generates random transect lines in a ring outside the windfarm footprint,
## rasterises them to seamask cell numbers, and produces a PreyMap raster with
## those cells' prey values bumped by a multiplier. Also reports the kJ of
## energy added to the system and provides a plotting helper.
##
## Source this file from your experiment script:
##   source("experiments/transect_helpers.R")
################################################################################

suppressPackageStartupMessages({
  library(raster)
  library(sf)
})


# ------------------------------------------------------------------------------
# make_transect_prey()
#
# Build a relative PreyMap with elevated prey along randomly-placed transect
# lines that sit between min_distance and max_distance metres outside the ORD
# footprint. Returns a list with the PreyMap raster, the transect geometry,
# the cell numbers that were bumped, and an energy accounting summary.
# ------------------------------------------------------------------------------
make_transect_prey <- function(seamask,
                               ORDpoly,
                               min_distance = 20000,
                               max_distance = 40000,
                               n_transects  = 5,
                               length_m     = 10000,
                               width_m      = 0,
                               multiplier   = 2.0,
                               Pmedian_value = NULL,
                               energy_prey   = NULL,
                               seed          = NULL) {

  stopifnot(inherits(seamask, "RasterLayer"))
  stopifnot(multiplier >= 1)
  stopifnot(min_distance < max_distance)

  if (!is.null(seed)) set.seed(seed)

  # --- 1. Build the ring zone (between min and max distance from windfarm) ---
  ord_union <- sf::st_union(ORDpoly)
  inner     <- sf::st_buffer(ord_union, dist = min_distance)
  outer     <- sf::st_buffer(ord_union, dist = max_distance)
  ring      <- sf::st_difference(outer, inner)

  # --- 2. Random midpoints inside the ring ---
  midpts <- sf::st_sample(ring, size = n_transects, type = "random")

  # --- 3. Random orientations and build linestrings around each midpoint ---
  angles   <- stats::runif(n_transects, 0, pi)
  half_len <- length_m / 2

  transect_list <- lapply(seq_len(n_transects), function(i) {
    p  <- sf::st_coordinates(midpts[i])
    dx <- half_len * cos(angles[i])
    dy <- half_len * sin(angles[i])
    sf::st_linestring(matrix(c(p[1] - dx, p[1] + dx,
                               p[2] - dy, p[2] + dy), ncol = 2))
  })
  transects_sfc <- sf::st_sfc(transect_list, crs = sf::st_crs(ORDpoly))

  # --- 4. Optional buffering into corridors ---
  geom <- if (width_m > 0) sf::st_buffer(transects_sfc, dist = width_m / 2) else transects_sfc

  # --- 5. Rasterise to cell numbers, restrict to sea (value 0 in this seamask) ---
  rasterized <- raster::rasterize(sf::as_Spatial(geom), seamask, field = 1)
  transect_cells <- which(!is.na(raster::values(rasterized)))
  sea_cells      <- which(raster::values(seamask) == 0)
  target_cells   <- intersect(transect_cells, sea_cells)

  # --- 6. Build the PreyMap: uniform 1 over sea, multiplier in target cells ---
  PreyMap <- raster::calc(seamask, fun = function(x) { x[x == 0] <- 1; x })
  v <- raster::values(PreyMap)
  v[target_cells] <- v[target_cells] * multiplier
  raster::values(PreyMap) <- v

  # --- 7. Energy accounting ---
  # The model computes PreyAvailable = PreyMap * Pmedian per replicate. So the
  # *extra* prey mass added per bumped cell is Pmedian * (multiplier - 1).
  # Multiplied by energy_prey (kJ/g) gives the total energetic injection.
  energy_summary <- NULL
  if (!is.null(Pmedian_value) && !is.null(energy_prey)) {
    extra_mass_per_cell_g <- Pmedian_value * (multiplier - 1)
    total_extra_mass_g    <- length(target_cells) * extra_mass_per_cell_g
    total_extra_kJ        <- total_extra_mass_g * energy_prey

    energy_summary <- list(
      n_bumped_cells        = length(target_cells),
      Pmedian               = Pmedian_value,
      multiplier            = multiplier,
      extra_mass_per_cell_g = extra_mass_per_cell_g,
      total_extra_mass_g    = total_extra_mass_g,
      energy_prey_kJperG    = energy_prey,
      total_extra_kJ        = total_extra_kJ
    )

    message(sprintf(
      "Energy accounting: %d cells bumped, Pmedian=%g g/cell, multiplier=%.2f -> +%.2f g/cell extra. Total +%.0f g of prey = +%.0f kJ.",
      length(target_cells), Pmedian_value, multiplier,
      extra_mass_per_cell_g, total_extra_mass_g, total_extra_kJ))
  }

  list(PreyMap        = PreyMap,
       transects      = transects_sfc,
       target_cells   = target_cells,
       ring           = ring,
       energy_summary = energy_summary)
}


# ------------------------------------------------------------------------------
# plot_preymap()
#
# Visualise the PreyMap raster with windfarm and transect overlays. Pass
# `file = "outputs/something.png"` to save to disk instead of plotting on screen.
# Set `zoom = TRUE` to crop to a tight bounding box around windfarm + transects
# (much more readable than the full UK-scale raster).
# ------------------------------------------------------------------------------
plot_preymap <- function(PreyMap, ORDpoly, transects = NULL, ring = NULL,
                         file = NULL, zoom = TRUE, title = "Prey distribution (relative)") {

  # Decide plotting extent
  if (zoom) {
    bb_features <- c(list(sf::st_geometry(ORDpoly)),
                     if (!is.null(transects)) list(sf::st_geometry(transects)) else list())
    bb <- sf::st_bbox(do.call(c, bb_features))
    pad <- max(diff(bb[c("xmin","xmax")]), diff(bb[c("ymin","ymax")])) * 0.2
    ext <- raster::extent(bb["xmin"] - pad, bb["xmax"] + pad,
                          bb["ymin"] - pad, bb["ymax"] + pad)
    PreyMap_plot <- raster::crop(PreyMap, ext)
  } else {
    PreyMap_plot <- PreyMap
  }

  if (!is.null(file)) {
    dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
    grDevices::png(file, width = 1200, height = 1000, res = 150)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  # Heatmap palette: pale where prey is 1, hot where prey > 1
  raster::plot(PreyMap_plot,
               main  = title,
               col   = grDevices::colorRampPalette(c("#e6f2ff", "#ffeb99", "#ff5050"))(50),
               colNA = "grey90")

  if (!is.null(ring)) {
    plot(sf::st_geometry(ring), add = TRUE, border = "grey50", lty = 2, lwd = 1)
  }
  plot(sf::st_geometry(ORDpoly), add = TRUE, border = "black", lwd = 2, col = NA)
  if (!is.null(transects)) {
    plot(sf::st_geometry(transects), add = TRUE, col = "darkgreen", lwd = 2)
  }

  graphics::legend("bottomright",
                   legend = c("Windfarm", "Transect", "Ring zone"),
                   col    = c("black", "darkgreen", "grey50"),
                   lty    = c(1, 1, 2),
                   lwd    = c(2, 2, 1),
                   bg     = "white", cex = 0.8)

  invisible(PreyMap_plot)
}


# ------------------------------------------------------------------------------
# Example usage (uncomment and adapt)
# ------------------------------------------------------------------------------
# library(seabORD)
# data("seamask_3035_example"); data("ORDpoly_example"); data("energeticsandpreydata")
#
# # Rebuild seamask raster (see vignette H_exampleKI_run.Rmd for the recipe)
# md <- seamask_3035_example$metadata
# seamask <- raster::raster(nrows = md$n_rows, ncols = md$n_cols,
#                           xmn = md$x_min, xmx = md$x_max,
#                           ymn = md$y_min, ymx = md$y_max,
#                           crs = md$crs) |>
#            raster::setValues(seamask_3035_example$matrix)
#
# # Get energy_prey for the species you're modelling
# spdat <- dplyr::filter(energeticsandpreydata, Code == "KI")
#
# # Build the transect-enriched PreyMap with energy accounting
# res <- make_transect_prey(
#   seamask       = seamask,
#   ORDpoly       = ORDpoly_example,
#   min_distance  = 20000,
#   max_distance  = 40000,
#   n_transects   = 10,
#   length_m      = 8000,
#   width_m       = 1000,
#   multiplier    = 2.0,
#   Pmedian_value = 250,                  # whatever Par$Pmedian is for the run
#   energy_prey   = spdat$energy_prey,
#   seed          = 2026
# )
#
# # Inspect the energy summary
# str(res$energy_summary)
#
# # Visualise on screen, zoomed to the windfarm + transects
# plot_preymap(res$PreyMap, ORDpoly_example,
#              transects = res$transects, ring = res$ring)
#
# # Or save to disk
# plot_preymap(res$PreyMap, ORDpoly_example,
#              transects = res$transects, ring = res$ring,
#              file = "outputs/preymap_transects.png")
