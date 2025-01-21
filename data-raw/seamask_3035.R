## code to prepare `seamask_3035` dataset goes here


#to handle rasters save both the raster data as a matrix and its metadata to then compose the raster back again
seamask_3035_r <- terra::rast("C:/Users/madtig/Documents/repositories/SeabORD/seabord-r-dev/Data/seamask_3035.tif")

seamask_3035_m <- terra::as.matrix(seamask_3035_r)

#save the metadata
nrows <- nrow(seamask_3035_r)
ncols <- ncol(seamask_3035_r)
xmin <- as.vector(ext(seamask_3035_r)[1])
xmax <- as.vector(ext(seamask_3035_r)[2])
ymin <- as.vector(ext(seamask_3035_r)[3])
ymax <- as.vector(ext(seamask_3035_r)[4])
crs <- crs(seamask_3035_r)

seamask_3035_metadata <- list(
  n_rows = nrows,
  n_cols = ncols,
  x_min = xmin,
  x_max = xmax,
  y_min = ymin,
  y_max = ymax,
  crs = crs
)


seamask_3035_example <- list(matrix = seamask_3035_m,
                     metadata = seamask_3035_metadata)

usethis::use_data(seamask_3035_example, overwrite = TRUE)
