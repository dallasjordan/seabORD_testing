## code to prepare `frgcompdata_example_dd` dataset goes here

frgcompdata <- raster::raster("C:/Users/chrpol/OneDrive - UKCEH/00000_SeabORD_Development/DD_compmap.tif")

frgcompdata_r <- terra::rast(frgcompdata)
frgcompdata_m <- terra::as.matrix(frgcompdata_r)

plot(frgcompdata_r)


#save the metadata
nrows <- nrow(frgcompdata_r)
ncols <- ncol(frgcompdata_r)
xmin <- as.vector(terra::ext(frgcompdata_r)[1])
xmax <- as.vector(terra::ext(frgcompdata_r)[2])
ymin <- as.vector(terra::ext(frgcompdata_r)[3])
ymax <- as.vector(terra::ext(frgcompdata_r)[4])
crs <- crs(frgcompdata_r)

frgcompdata_metadata <- list(
  n_rows = nrows,
  n_cols = ncols,
  x_min = xmin,
  x_max = xmax,
  y_min = ymin,
  y_max = ymax,
  crs = crs
)


frgcompdata_example_dd <- list(matrix = frgcompdata_m,
                            metadata = frgcompdata_metadata)


usethis::use_data(frgcompdata_example_dd, overwrite = TRUE)

