## code to prepare `UK9004171_bysea_3035` dataset goes here

UK9004171_bysea_3035_r <- terra::rast("C:/Users/madtig/Documents/repositories/SeabORD/seabord-r-dev/Data/UK9004171_bysea_3035.tif")
UK9004171_bysea_3035_m <- terra::as.matrix(UK9004171_bysea_3035_r)

plot(UK9004171_bysea_3035_r)

#save the metadata
nrows <- nrow(UK9004171_bysea_3035_r)
ncols <- ncol(UK9004171_bysea_3035_r)
xmin <- as.vector(ext(UK9004171_bysea_3035_r)[1])
xmax <- as.vector(ext(UK9004171_bysea_3035_r)[2])
ymin <- as.vector(ext(UK9004171_bysea_3035_r)[3])
ymax <- as.vector(ext(UK9004171_bysea_3035_r)[4])
crs <- crs(UK9004171_bysea_3035_r)


UK9004171_bysea_3035_metadata <- list(
  n_rows = nrows,
  n_cols = ncols,
  x_min = xmin,
  x_max = xmax,
  y_min = ymin,
  y_max = ymax,
  crs = crs
)

UK9004171_bysea_3035 <- list(matrix = UK9004171_bysea_3035_m,
                             metadata = UK9004171_bysea_3035_metadata)

usethis::use_data(UK9004171_bysea_3035, overwrite = TRUE)
