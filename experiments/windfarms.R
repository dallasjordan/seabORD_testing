################################################################################
## Load the Forth/Tay offshore windfarm footprints for seabORD.
##
## Reads the shapefile of 4 windfarms (Inch Cape, Seagreen, Neart na Gaoithe,
## Berwick Bank), reprojects to the seamask CRS (EPSG:3035), and returns both
## the ORDpoly sf object AND the matching `include_ORDs` label vector.
##
## IMPORTANT: seabORD couples ORDpoly and ordPar$include_ORDs *positionally* --
## it iterates `seq(length(include_ORDs))` and indexes the i-th polygon of
## ORDpoly (functions-seabordmain.R ~line 359, 430, 470). So include_ORDs must
## have the SAME length and ORDER as the rows of ORDpoly. This function keeps
## them in sync for you.
################################################################################

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
})

# ------------------------------------------------------------------------------
# load_windfarms()
#
# @param shp_path  path to WindfarmsForSeabORD.shp (raw footprints; seabORD adds
#                  its own FootprintBorder + BufferZone from ordPar, so do NOT
#                  feed it the pre-buffered shapefile or you double-buffer).
# @param target_crs CRS to reproject to (default: seamask EPSG:3035).
# @param include    optional character vector of windfarm short-codes to keep
#                   (subset). Default keeps all 4.
# @return list(ORDpoly = sf, include_ORDs = character, table = tibble)
# ------------------------------------------------------------------------------
load_windfarms <- function(shp_path,
                           target_crs = NULL,
                           include = c("INCAP", "SEAGREEN", "NEART", "BERWICK")) {

  wf <- sf::st_read(shp_path, quiet = TRUE)

  # Map the shapefile's descriptive names to short codes seabORD will use as labels.
  # Matching is done on the cleaned Name___ column.
  name_lookup <- c(
    "Inch Cape Offshore Wind Farm" = "INCAP",
    "Seagreen Phase 1 Windfarm"    = "SEAGREEN",
    "Neart na Gaoithe Offshore WF" = "NEART",
    "Berwick Bank Wind Farm"       = "BERWICK"
  )

  wf$short_code <- name_lookup[wf$Name___]
  if (any(is.na(wf$short_code))) {
    stop("load_windfarms: unrecognised windfarm name(s): ",
         paste(unique(wf$Name___[is.na(wf$short_code)]), collapse = ", "),
         "\nUpdate name_lookup in experiments/windfarms.R.")
  }

  # Keep only requested windfarms, in a deterministic order (the order of `include`)
  wf <- wf[wf$short_code %in% include, ]
  wf <- wf[match(include[include %in% wf$short_code], wf$short_code), ]

  # Reproject to the seamask CRS
  if (!is.null(target_crs)) {
    wf <- sf::st_transform(wf, crs = target_crs)
  }

  # Keep a tidy geometry-only sf plus a small attribute table for reference
  keep_cols <- intersect(c("short_code", "Name___", "Status___", "Capacity_M", "km2"),
                         names(wf))
  ORDpoly <- wf[, keep_cols]

  include_ORDs <- ORDpoly$short_code

  message(sprintf("Loaded %d windfarm(s): %s",
                  nrow(ORDpoly), paste(include_ORDs, collapse = ", ")))

  list(
    ORDpoly      = ORDpoly,
    include_ORDs = include_ORDs,
    table        = sf::st_drop_geometry(ORDpoly)
  )
}
