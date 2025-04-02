## code to prepare `UK9002491_bysea_3035` dataset goes here

UK9002491_bysea_3035_r <- terra::rast("C:/Users/chrpol/OneDrive - UKCEH/00000_SeabORD_Development/DBS_map.tif")
UK9002491_bysea_3035_m <- terra::as.matrix(UK9002491_bysea_3035_r)

plot(UK9002491_bysea_3035_r)

#save the metadata
nrows <- nrow(UK9002491_bysea_3035_r)
ncols <- ncol(UK9002491_bysea_3035_r)
xmin <- as.vector(terra::ext(UK9002491_bysea_3035_r)[1])
xmax <- as.vector(terra::ext(UK9002491_bysea_3035_r)[2])
ymin <- as.vector(terra::ext(UK9002491_bysea_3035_r)[3])
ymax <- as.vector(terra::ext(UK9002491_bysea_3035_r)[4])
crs <- crs(UK9002491_bysea_3035_r)


UK9002491_bysea_3035_metadata <- list(
  n_rows = nrows,
  n_cols = ncols,
  x_min = xmin,
  x_max = xmax,
  y_min = ymin,
  y_max = ymax,
  crs = crs
)

UK9002491_bysea_3035 <- list(matrix = UK9002491_bysea_3035_m,
                             metadata = UK9002491_bysea_3035_metadata)


usethis::use_data(UK9002491_bysea_3035, overwrite = TRUE)

