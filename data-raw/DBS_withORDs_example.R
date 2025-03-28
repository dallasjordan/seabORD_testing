## code to prepare `DBS_withORDs_example` dataset goes here

DBS_withORDs <- terra::rast("C:/Users/chrpol/OneDrive - UKCEH/00000_SeabORD_Development/DBS_withORDs.tif")

DBS_withORDs_m <- terra::as.matrix(DBS_withORDs)

#save the metadata
nrows <- nrow(DBS_withORDs)
ncols <- ncol(DBS_withORDs)
xmin <- as.vector(terra::ext(DBS_withORDs)[1])
xmax <- as.vector(terra::ext(DBS_withORDs)[2])
ymin <- as.vector(terra::ext(DBS_withORDs)[3])
ymax <- as.vector(terra::ext(DBS_withORDs)[4])
crs <- terra::crs(DBS_withORDs)

DBS_withORDs_metadata <- list(
  n_rows = nrows,
  n_cols = ncols,
  x_min = xmin,
  x_max = xmax,
  y_min = ymin,
  y_max = ymax,
  crs = crs
)


DBS_withORDs_example <- list(matrix = DBS_withORDs_m,
                        metadata = DBS_withORDs_metadata)


usethis::use_data(DBS_withORDs_example, overwrite = TRUE)
