# read raster data; 50m landuse of 2020
landuse <- terra::rast("../GIS_data/geotiff_clum_50m1220m/clum_50m1220m.tif")

# a function to change the raster codes to a binary agriculture
agri_codes <- function(x){
  # codes for agriculature classes
  code <- c(
    # dryland and irrigaed cropping
    330, 331, 332, 333, 334, 335, 336,
    337, 338, 430, 431, 432, 433, 434, 
    435, 436, 437, 438,
    # Grazing irrigated modified pastures
    400, 420, 421, 422, 423, 424
  )
  y <- ifelse(x %in% code, 1, 0)
  return(y)
}

# convert the landuse to agri/non-agri
agriculture <- terra::app(x = landuse, 
                          # cores = 4, 
                          fun = agri_codes,
                          filename = "spatial_data/agri_50m.tif",
                          overwrite = TRUE)