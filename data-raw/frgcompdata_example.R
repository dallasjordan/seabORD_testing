## code to prepare `frgcompdata_example` dataset goes here


## code to prepare `BrdData_exmaple` dataset goes here

frgcompdata <- raster::raster("C:/Users/madtig/Documents/repositories/SeabORD/seabord-r-dev/Data/NewSeanseMapsForSeabordR/seanse-for-seabordr-compmaps-KI-ForthIslands")

frgcompdata_r <- terra::rast(frgcompdata)
frgcompdata_m <- terra::as.matrix(frgcompdata_r)

plot(frgcompdata_r)


#save the metadata
nrows <- nrow(frgcompdata_r)
ncols <- ncol(frgcompdata_r)
xmin <- as.vector(ext(frgcompdata_r)[1])
xmax <- as.vector(ext(frgcompdata_r)[2])
ymin <- as.vector(ext(frgcompdata_r)[3])
ymax <- as.vector(ext(frgcompdata_r)[4])
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


frgcompdata_example <- list(matrix = frgcompdata_m,
                        metadata = frgcompdata_metadata)

usethis::use_data(frgcompdata_example, overwrite = TRUE)
