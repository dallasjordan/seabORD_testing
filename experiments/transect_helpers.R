################################################################################
## Helpers for building spatial prey-enrichment scenarios (PreyMap rasters).
##
## Provides:
##   resolve_injection()  - convert a desired multiplier / biomass / kJ into a
##                          per-cell PreyMap multiplier, with full energy
##                          accounting (both the model's energy density and any
##                          user-assumed density).
##   make_point_prey()    - enrich a SINGLE cell (a "feeding station") at a
##                          chosen or auto-selected reachable location.
##   make_transect_prey() - enrich random transect lines in a ring outside the
##                          windfarm footprint.
##   diagnose_preymap()   - check whether birds can reach the enriched cells
##                          (BrdData overlap) and whether prey is limiting
##                          (Michaelis-Menten saturation check).
##   plot_preymap()       - visualise a PreyMap with windfarm / feature overlays.
##
## KEY MODEL SEMANTICS (read before designing a scenario):
##  * PreyMap values are UNITLESS RELATIVE multipliers. Absolute prey mass in a
##    cell = PreyMap_value * Par$Pmedian[simrun]  (grams per 1 km cell).
##  * PreyAvailable is a per-TIMESTEP standing density that RESETS every
##    timestep. It is NOT a depletable seasonal stock. Enriching a cell makes
##    that biomass available *every* timestep, i.e. an unlimited feeding station.
##  * When a bird forages, energy gained = grams_caught * spdat$energy_prey.
##    So the MODEL's energy density (e.g. KI = 6.52 kJ/g) governs realised
##    energy, regardless of any density you assume when sizing the injection.
################################################################################

suppressPackageStartupMessages({
  library(raster)
  library(sf)
})


# ------------------------------------------------------------------------------
# resolve_injection()
#
# Convert a desired enrichment (specified ONE of three ways) into a per-cell
# PreyMap multiplier plus a full accounting table.
#
# Specify exactly one of:
#   multiplier    - relative multiplier applied to each target cell (>= 1)
#   target_mass_g - total biomass (grams) to spread across the target cells
#   target_kJ     - total energy (kJ); converted to grams via target_energy_density
#                   (if given) else energy_prey_model.
#
# @param n_cells            number of target cells the injection is spread over
# @param Pmedian            baseline prey mass per cell (g), i.e. Par$Pmedian
# @param energy_prey_model  the MODEL's prey energy density (kJ/g), e.g. spdat$energy_prey
# @param target_energy_density  optional user-assumed density (kJ/g) for target_kJ
# ------------------------------------------------------------------------------
resolve_injection <- function(n_cells, Pmedian, energy_prey_model,
                              multiplier = NULL,
                              target_mass_g = NULL,
                              target_kJ = NULL,
                              target_energy_density = NULL) {

  specified <- c(multiplier = !is.null(multiplier),
                 target_mass_g = !is.null(target_mass_g),
                 target_kJ = !is.null(target_kJ))
  if (sum(specified) != 1) {
    stop("resolve_injection: specify EXACTLY one of multiplier, target_mass_g, target_kJ.")
  }
  stopifnot(n_cells >= 1, Pmedian > 0)

  user_energy_density <- NA_real_

  if (!is.null(multiplier)) {
    stopifnot(multiplier >= 1)
    grams_per_cell <- Pmedian * (multiplier - 1)
    total_mass_g   <- grams_per_cell * n_cells
  } else if (!is.null(target_mass_g)) {
    total_mass_g   <- target_mass_g
    grams_per_cell <- total_mass_g / n_cells
    multiplier     <- 1 + grams_per_cell / Pmedian
  } else { # target_kJ
    ed <- if (!is.null(target_energy_density)) target_energy_density else energy_prey_model
    user_energy_density <- ed
    total_mass_g   <- target_kJ / ed
    grams_per_cell <- total_mass_g / n_cells
    multiplier     <- 1 + grams_per_cell / Pmedian
  }

  list(
    multiplier          = multiplier,
    n_cells             = n_cells,
    Pmedian             = Pmedian,
    grams_per_cell      = grams_per_cell,
    total_mass_g        = total_mass_g,
    total_mass_kg       = total_mass_g / 1000,
    energy_prey_model   = energy_prey_model,
    user_energy_density = user_energy_density,
    total_kJ_model      = total_mass_g * energy_prey_model,
    total_kJ_user       = if (!is.na(user_energy_density)) total_mass_g * user_energy_density else NA_real_
  )
}

# Pretty-print an injection accounting object.
print_injection <- function(inj) {
  cat("=== Prey injection accounting ===\n")
  cat(sprintf("Target cells           : %d\n", inj$n_cells))
  cat(sprintf("Baseline Pmedian       : %g g/cell\n", inj$Pmedian))
  cat(sprintf("PreyMap multiplier     : %.2f  (per target cell)\n", inj$multiplier))
  cat(sprintf("Biomass added / cell   : %s g\n", format(round(inj$grams_per_cell), big.mark=",")))
  cat(sprintf("Biomass added (total)  : %s g  = %s kg\n",
              format(round(inj$total_mass_g), big.mark=","),
              format(round(inj$total_mass_kg), big.mark=",")))
  cat(sprintf("Default energy density : %.2f kJ/g  (spdat$energy_prey, rest of sea)\n", inj$energy_prey_model))
  if (!is.null(inj$offal_energy_density)) {
    # Offal cell overrides the default via the EnergyMap.
    cat(sprintf("Offal energy density   : %.2f kJ/g  (EnergyMap override IN THIS CELL)\n",
                inj$offal_energy_density))
    cat(sprintf("=> Energy available in the offal cell: %s kJ  (biomass * offal density)\n",
                format(round(inj$cell_energy_kJ_offal), big.mark=",")))
  } else {
    cat(sprintf("=> Energy available if fully eaten: %s kJ  (biomass * default density)\n",
                format(round(inj$total_kJ_model), big.mark=",")))
  }
  if (!is.na(inj$user_energy_density)) {
    cat(sprintf("Your assumed density   : %.2f kJ/g\n", inj$user_energy_density))
    cat(sprintf("=> Your-accounting energy: %s kJ\n",
                format(round(inj$total_kJ_user), big.mark=",")))
    if (abs(inj$user_energy_density - inj$energy_prey_model) > 1e-6) {
      cat("NOTE: your assumed density differs from the model's. Birds extract energy\n")
      cat("      at the MODEL's density; the biomass (grams) is the fixed quantity.\n")
    }
  }
  invisible(inj)
}


# ------------------------------------------------------------------------------
# make_point_prey()
#
# Enrich a single seamask cell. Location is either supplied (c(x, y) in the
# seamask CRS) or auto-picked as the most-visited (highest BrdData) reachable
# sea cell inside a ring `min_distance`-`max_distance` m outside the windfarm.
#
# The enrichment size (biomass) is specified via resolve_injection() args
# (multiplier / target_mass_g / target_kJ). For higher-quality prey such as
# offal, set offal_energy_density (kJ/g): this returns an EnergyMap so that birds
# foraging in the enriched cell extract that many kJ per gram (instead of the
# species default). Pass BOTH PreyMap and EnergyMap to seabord().
#
# Example (dump 2000 kg of offal at 9 kJ/g):
#   make_point_prey(..., target_mass_g = 2000*1000, offal_energy_density = 9)
# ------------------------------------------------------------------------------
# `center` (an sf point, e.g. the colony) replaces the windfarm as the centre of
# the search zone; use min_distance = 0 for a solid disc of radius max_distance.
make_point_prey <- function(seamask, ORDpoly = NULL,
                            Pmedian_value, energy_prey_model,
                            location = NULL,
                            center = NULL,
                            BrdData = NULL,
                            min_distance = 20000,
                            max_distance = 40000,
                            multiplier = NULL,
                            target_mass_g = NULL,
                            target_kJ = NULL,
                            target_energy_density = NULL,
                            offal_energy_density = NULL) {

  stopifnot(inherits(seamask, "RasterLayer"))
  sea_cells <- which(raster::values(seamask) == 0)

  # --- Determine the target cell ---
  if (!is.null(location)) {
    target_cell <- raster::cellFromXY(seamask, matrix(location, ncol = 2))
    if (is.na(target_cell) || !(target_cell %in% sea_cells)) {
      stop("make_point_prey: supplied location is not a sea cell.")
    }
    ring <- NULL
  } else {
    if (is.null(BrdData)) {
      stop("make_point_prey: provide BrdData to auto-pick a reachable cell, or supply an explicit location.")
    }
    if (is.null(center) && is.null(ORDpoly)) {
      stop("make_point_prey: provide either ORDpoly or center to define the search zone.")
    }
    base_geom <- if (!is.null(center)) sf::st_geometry(center) else sf::st_union(ORDpoly)
    outer <- sf::st_buffer(base_geom, dist = max_distance)
    ring  <- if (min_distance > 0)
               sf::st_difference(outer, sf::st_buffer(base_geom, dist = min_distance)) else outer
    ring_r <- raster::rasterize(sf::as_Spatial(ring), seamask, field = 1)
    ring_cells <- which(!is.na(raster::values(ring_r)))

    brd_v <- raster::values(BrdData)
    reachable <- which(!is.na(brd_v) & brd_v > 0)
    cand <- intersect(intersect(ring_cells, sea_cells), reachable)
    if (length(cand) == 0) {
      stop("make_point_prey: no reachable (BrdData>0) sea cells in the ring; widen the ring or supply an explicit location.")
    }
    target_cell <- cand[which.max(brd_v[cand])]  # most-visited reachable cell
  }

  # --- Resolve the injection to a per-cell multiplier ---
  inj <- resolve_injection(n_cells = 1, Pmedian = Pmedian_value,
                           energy_prey_model = energy_prey_model,
                           multiplier = multiplier,
                           target_mass_g = target_mass_g,
                           target_kJ = target_kJ,
                           target_energy_density = target_energy_density)

  # --- Build PreyMap directly on the seamask grid (no reprojection/smearing) ---
  PreyMap <- raster::calc(seamask, fun = function(x) { x[x == 0] <- 1; x })
  v <- raster::values(PreyMap)
  v[target_cell] <- inj$multiplier
  raster::values(PreyMap) <- v

  # --- Optional EnergyMap: give the enriched cell a different prey quality ---
  # NA everywhere (falls back to species density in the model) except the
  # target cell, which gets offal_energy_density kJ/g.
  EnergyMap <- NULL
  if (!is.null(offal_energy_density)) {
    EnergyMap <- seamask
    raster::values(EnergyMap) <- NA_real_
    EnergyMap[target_cell] <- offal_energy_density
    # Energy actually made available in the cell = biomass (g) * offal density.
    inj$offal_energy_density <- offal_energy_density
    inj$cell_energy_kJ_offal <- inj$total_mass_g * offal_energy_density
  }

  xy <- raster::xyFromCell(seamask, target_cell)
  point <- sf::st_sfc(sf::st_point(as.numeric(xy)), crs = sf::st_crs(ORDpoly))

  message(sprintf("Point enrichment at cell %d (x=%.0f, y=%.0f): multiplier=%.1f%s",
                  target_cell, xy[1], xy[2], inj$multiplier,
                  if (!is.null(offal_energy_density))
                    sprintf(", offal @ %.1f kJ/g", offal_energy_density) else ""))

  list(PreyMap = PreyMap, EnergyMap = EnergyMap,
       target_cell = target_cell, target_cells = target_cell,
       point = point, ring = ring, injection = inj)
}


# ------------------------------------------------------------------------------
# make_transect_prey()
#
# Enrich random transect lines in a ring `min_distance`-`max_distance` m outside
# the windfarm. Enrichment size via resolve_injection() (multiplier / mass / kJ).
# When target_mass_g or target_kJ is used, the total is spread across ALL
# transect cells (so per-cell multiplier depends on how many cells result).
# ------------------------------------------------------------------------------
make_transect_prey <- function(seamask, ORDpoly,
                               Pmedian_value, energy_prey_model,
                               min_distance = 20000,
                               max_distance = 40000,
                               n_transects  = 5,
                               length_m     = 10000,
                               width_m      = 0,
                               sea_only     = TRUE,
                               max_attempts_per_transect = 200,
                               multiplier   = NULL,
                               target_mass_g = NULL,
                               target_kJ = NULL,
                               target_energy_density = NULL,
                               seed = NULL) {

  stopifnot(inherits(seamask, "RasterLayer"), min_distance < max_distance)
  if (!is.null(seed)) set.seed(seed)

  ord_union <- sf::st_union(ORDpoly)
  inner     <- sf::st_buffer(ord_union, dist = min_distance)
  outer     <- sf::st_buffer(ord_union, dist = max_distance)
  ring      <- sf::st_difference(outer, inner)

  sea_cells <- which(raster::values(seamask) == 0)
  half_len  <- length_m / 2
  ring_crs  <- sf::st_crs(ORDpoly)

  draw_one <- function() {
    midpt <- sf::st_sample(ring, size = 1, type = "random")
    angle <- stats::runif(1, 0, pi)
    p     <- sf::st_coordinates(midpt)
    dx    <- half_len * cos(angle); dy <- half_len * sin(angle)
    sf::st_linestring(matrix(c(p[1] - dx, p[1] + dx, p[2] - dy, p[2] + dy), ncol = 2))
  }

  transect_list <- vector("list", n_transects)
  cells_per     <- vector("list", n_transects)
  attempts_total <- 0L

  for (i in seq_len(n_transects)) {
    accepted <- FALSE
    for (k in seq_len(max_attempts_per_transect)) {
      attempts_total <- attempts_total + 1L
      line <- draw_one()
      line_sfc <- sf::st_sfc(list(line), crs = ring_crs)
      geom_one <- if (width_m > 0) sf::st_buffer(line_sfc, dist = width_m / 2) else line_sfc
      rast_one <- raster::rasterize(sf::as_Spatial(geom_one), seamask, field = 1)
      cell_idx <- which(!is.na(raster::values(rast_one)))
      if (length(cell_idx) == 0) next
      if (sea_only && !all(cell_idx %in% sea_cells)) next
      transect_list[[i]] <- line
      cells_per[[i]]     <- intersect(cell_idx, sea_cells)
      accepted <- TRUE
      break
    }
    if (!accepted) {
      stop(sprintf("make_transect_prey: could not place transect %d over sea after %d attempts.",
                   i, max_attempts_per_transect))
    }
  }

  transects_sfc <- sf::st_sfc(transect_list, crs = ring_crs)
  target_cells  <- unique(unlist(cells_per))

  inj <- resolve_injection(n_cells = length(target_cells), Pmedian = Pmedian_value,
                           energy_prey_model = energy_prey_model,
                           multiplier = multiplier, target_mass_g = target_mass_g,
                           target_kJ = target_kJ, target_energy_density = target_energy_density)

  PreyMap <- raster::calc(seamask, fun = function(x) { x[x == 0] <- 1; x })
  v <- raster::values(PreyMap)
  v[target_cells] <- inj$multiplier
  raster::values(PreyMap) <- v

  message(sprintf("Placed %d transect(s) in %d draws; %d enriched cells, multiplier=%.2f.",
                  n_transects, attempts_total, length(target_cells), inj$multiplier))

  list(PreyMap = PreyMap, transects = transects_sfc, target_cells = target_cells,
       ring = ring, injection = inj)
}


# ------------------------------------------------------------------------------
# make_area_prey()
#
# Enrich EVERY sea cell in a broad zone outside the windfarm (a ring between
# min_distance and max_distance m). This is the design to use when you need a
# DETECTABLE population effect: a single cell is reached by ~0.04% of foraging
# pressure, whereas a broad zone can capture a meaningful share.
#
# Set reachable_only = TRUE to enrich only cells birds actually use (BrdData > 0);
# this concentrates the injected biomass where it will be encountered.
#
# Enrichment size via resolve_injection() (multiplier / target_mass_g / target_kJ).
# With a multiplier, every zone cell is scaled by it. With target_mass_g/target_kJ,
# the total is spread evenly across the zone cells.
#
# The zone is an annulus min_distance..max_distance around a CENTRE. By default
# the centre is the windfarm footprint (pass ORDpoly). To enrich near a colony
# instead, pass `center` (an sf point/geometry, e.g. the Isle of May) and use
# min_distance = 0 for a solid disc of radius max_distance.
#
# For offal (higher-quality prey), set offal_energy_density (kJ/g): the returned
# EnergyMap marks the enriched cells at that density (birds there extract it).
# ------------------------------------------------------------------------------
make_area_prey <- function(seamask, ORDpoly = NULL,
                           Pmedian_value, energy_prey_model,
                           center = NULL,
                           min_distance = 20000,
                           max_distance = 40000,
                           reachable_only = TRUE,
                           BrdData = NULL,
                           multiplier = NULL,
                           target_mass_g = NULL,
                           target_kJ = NULL,
                           target_energy_density = NULL,
                           offal_energy_density = NULL) {

  stopifnot(inherits(seamask, "RasterLayer"), min_distance < max_distance)
  if (reachable_only && is.null(BrdData)) {
    stop("make_area_prey: reachable_only = TRUE needs BrdData.")
  }
  if (is.null(center) && is.null(ORDpoly)) {
    stop("make_area_prey: provide either ORDpoly or center to define the zone.")
  }

  base_geom <- if (!is.null(center)) sf::st_geometry(center) else sf::st_union(ORDpoly)
  outer <- sf::st_buffer(base_geom, dist = max_distance)
  zone  <- if (min_distance > 0) sf::st_difference(outer, sf::st_buffer(base_geom, min_distance)) else outer

  zone_r <- raster::rasterize(sf::as_Spatial(zone), seamask, field = 1)
  zone_cells <- which(!is.na(raster::values(zone_r)))
  sea_cells  <- which(raster::values(seamask) == 0)
  target_cells <- intersect(zone_cells, sea_cells)

  if (reachable_only) {
    brd_v <- raster::values(BrdData)
    reachable <- which(!is.na(brd_v) & brd_v > 0)
    target_cells <- intersect(target_cells, reachable)
  }
  if (length(target_cells) == 0) stop("make_area_prey: no target cells in zone.")

  inj <- resolve_injection(n_cells = length(target_cells), Pmedian = Pmedian_value,
                           energy_prey_model = energy_prey_model,
                           multiplier = multiplier, target_mass_g = target_mass_g,
                           target_kJ = target_kJ, target_energy_density = target_energy_density)

  PreyMap <- raster::calc(seamask, fun = function(x) { x[x == 0] <- 1; x })
  v <- raster::values(PreyMap)
  v[target_cells] <- inj$multiplier
  raster::values(PreyMap) <- v

  # Optional EnergyMap: mark enriched cells with offal energy density (else NA
  # -> species default in the model).
  EnergyMap <- NULL
  if (!is.null(offal_energy_density)) {
    EnergyMap <- seamask
    raster::values(EnergyMap) <- NA_real_
    EnergyMap[target_cells] <- offal_energy_density
    inj$offal_energy_density <- offal_energy_density
    inj$cell_energy_kJ_offal <- inj$total_mass_g * offal_energy_density
  }

  message(sprintf("Area enrichment: %d cells (reachable_only=%s), multiplier=%.2f%s.",
                  length(target_cells), reachable_only, inj$multiplier,
                  if (!is.null(offal_energy_density)) sprintf(", offal @ %.1f kJ/g", offal_energy_density) else ""))

  list(PreyMap = PreyMap, EnergyMap = EnergyMap,
       target_cells = target_cells, zone = zone, injection = inj)
}


# ------------------------------------------------------------------------------
# diagnose_preymap()
#
# Two checks that determine whether an enrichment scenario can produce a signal:
#   1. Reachability: do the enriched cells overlap where birds actually forage
#      (BrdData > 0), and what fraction of total foraging pressure reaches them?
#   2. Saturation: is baseline prey below/above the Michaelis-Menten half-
#      saturation constant (IR_half_a)? Below => prey limiting => adding prey
#      matters. Above => intake near-saturated => adding prey does little.
# ------------------------------------------------------------------------------
diagnose_preymap <- function(PreyMap, BrdData, seamask, spdat, Pmedian_value,
                             target_cells = NULL) {
  sea <- which(raster::values(seamask) == 0)
  brd <- raster::values(BrdData); brd[is.na(brd)] <- 0
  pm  <- raster::values(PreyMap)
  if (is.null(target_cells)) target_cells <- which(pm > 1)

  cat("=== Reachability (do birds reach the enriched cells?) ===\n")
  cat(sprintf("Enriched cells                         : %d\n", length(target_cells)))
  cat(sprintf("... of which reachable (BrdData > 0)   : %d\n", sum(brd[target_cells] > 0)))
  tot_brd <- sum(brd[sea])
  frac <- if (tot_brd > 0) sum(brd[target_cells]) / tot_brd else 0
  cat(sprintf("Share of total foraging pressure here  : %.4f%%\n", 100 * frac))
  if (sum(brd[target_cells] > 0) == 0) {
    cat("*** WARNING: no enriched cell is reachable. Birds will never forage here;\n")
    cat("*** the treatment will be invisible. Relocate the enrichment.\n")
  }

  cat("\n=== Saturation (is prey limiting at baseline?) ===\n")
  ratio <- Pmedian_value / spdat$IR_half_a
  cat(sprintf("Pmedian = %g g/cell ; IR_half_a = %g g ; ratio = %.3f\n",
              Pmedian_value, spdat$IR_half_a, ratio))
  if (ratio < 0.5) {
    cat("=> Baseline prey WELL BELOW half-saturation: prey is limiting; adding prey should have a clear effect.\n")
  } else if (ratio < 2) {
    cat("=> Baseline prey NEAR half-saturation: moderate sensitivity to added prey.\n")
  } else {
    cat("=> Baseline prey ABOVE half-saturation: intake near-saturated; added prey may do little.\n")
  }
  invisible(list(reachable_fraction = frac,
                 n_reachable = sum(brd[target_cells] > 0),
                 saturation_ratio = ratio))
}


# ------------------------------------------------------------------------------
# plot_preymap()
#
# Visualise a PreyMap with windfarm and feature overlays. `features` is any sf
# geometry to overlay (transect lines or a point); pass file= to save to PNG.
# ------------------------------------------------------------------------------
plot_preymap <- function(PreyMap, ORDpoly, features = NULL, ring = NULL,
                         file = NULL, zoom = TRUE,
                         title = "Prey distribution (relative)") {

  if (zoom) {
    geoms <- list(sf::st_geometry(ORDpoly))
    if (!is.null(features)) geoms <- c(geoms, list(sf::st_geometry(features)))
    bb  <- sf::st_bbox(do.call(c, geoms))
    pad <- max(diff(bb[c("xmin","xmax")]), diff(bb[c("ymin","ymax")])) * 0.25
    ext <- raster::extent(bb["xmin"] - pad, bb["xmax"] + pad,
                          bb["ymin"] - pad, bb["ymax"] + pad)
    PreyMap_plot <- raster::crop(PreyMap, ext)
  } else {
    PreyMap_plot <- PreyMap
  }

  # Log-scale the colour so a huge single-cell spike doesn't flatten everything.
  PreyMap_log <- raster::calc(PreyMap_plot, fun = function(x) log10(x))

  if (!is.null(file)) {
    dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
    grDevices::png(file, width = 1200, height = 1000, res = 150)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  raster::plot(PreyMap_log,
               main  = title,
               col   = grDevices::colorRampPalette(c("#e6f2ff", "#ffeb99", "#ff5050"))(50),
               colNA = "grey90",
               legend.args = list(text = "log10(relative prey)", side = 4, line = 2.5, cex = 0.8))

  if (!is.null(ring)) plot(sf::st_geometry(ring), add = TRUE, border = "grey50", lty = 2)
  plot(sf::st_geometry(ORDpoly), add = TRUE, border = "black", lwd = 2, col = NA)
  if (!is.null(features)) {
    plot(sf::st_geometry(features), add = TRUE, col = "darkgreen", lwd = 3, pch = 4, cex = 2)
  }
  invisible(PreyMap_plot)
}
