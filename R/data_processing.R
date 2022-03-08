library(tidyverse)
library(terra)
library(sf)


# read and crop climatic data ---------------------------------------------
# crop data to AU and write them
fld <- "C:/Users/61423/Cesar_projects/SugarPestsSDM/data/raster_layers"
li <- list.files(fld, pattern = ".tif$", full.names = TRUE)
rc <- rast(li)
rc
plot(rc[[1]])

border <- sf::st_read("spatial_data/aus_border.gpkg")
plot(st_geometry(border), add = TRUE)


# crop to Australian border
rcc <- terra::crop(rc, vect(border), mask = TRUE)
plot(rcc[[1:3]])
# write the clipped files
for(i in 1:nlyr(rcc)){
  nm <- names(rcc)[i]
  name <- paste0("spatial_data/", nm, ".tif")
  terra::writeRaster(rcc[[i]], filename = name, overwrite = TRUE)
  print(nm)
}


# create agriculture intensity --------------------------------------------
# read raster data; 50m landuse of 2020
landuse <- terra::rast("../GIS_data/geotiff_clum_50m1220m/clum_50m1220m.tif")
plot(landuse)

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

