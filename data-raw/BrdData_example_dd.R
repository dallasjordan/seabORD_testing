## code to prepare `BrdData_example_dd` dataset goes here


BrdData <- raster::raster("C:/Users/chrpol/OneDrive - UKCEH/00000_SeabORD_Development/DD_map.tif")


BrdData_r <- terra::rast(BrdData)
BrdData_m <- terra::as.matrix(BrdData_r)

plot(BrdData_r)


#save the metadata
nrows <- nrow(BrdData_r)
ncols <- ncol(BrdData_r)
xmin <- as.vector(terra::ext(BrdData_r)[1])
xmax <- as.vector(terra::ext(BrdData_r)[2])
ymin <- as.vector(terra::ext(BrdData_r)[3])
ymax <- as.vector(terra::ext(BrdData_r)[4])
crs <- crs(BrdData_r)

BrdData_metadata <- list(
  n_rows = nrows,
  n_cols = ncols,
  x_min = xmin,
  x_max = xmax,
  y_min = ymin,
  y_max = ymax,
  crs = crs
)


BrdData_example_dd <- list(matrix = BrdData_m,
                        metadata = BrdData_metadata)


usethis::use_data(BrdData_example_dd, overwrite = TRUE)
