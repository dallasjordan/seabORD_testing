## code to prepare `DBS_map_example` dataset goes here

DBS_map <- terra::rast("C:/Users/chrpol/OneDrive - UKCEH/00000_SeabORD_Development/DBS_map.tif")

DBS_map_m <- terra::as.matrix(DBS_map)

#save the metadata
nrows <- nrow(DBS_map)
ncols <- ncol(DBS_map)
xmin <- as.vector(terra::ext(DBS_map)[1])
xmax <- as.vector(terra::ext(DBS_map)[2])
ymin <- as.vector(terra::ext(DBS_map)[3])
ymax <- as.vector(terra::ext(DBS_map)[4])
crs <- terra::crs(DBS_map)

DBS_map_metadata <- list(
  n_rows = nrows,
  n_cols = ncols,
  x_min = xmin,
  x_max = xmax,
  y_min = ymin,
  y_max = ymax,
  crs = crs
)


DBS_map_example <- list(matrix = DBS_map_m,
                        metadata = DBS_map_metadata)

usethis::use_data(DBS_map_example, overwrite = TRUE)
