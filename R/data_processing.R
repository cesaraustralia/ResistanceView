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


# read the climate data
rc_proj <- rast(list.files("spatial_data/climate_data/", full.names = TRUE)) %>% 
  terra::project(y = terra::crs(landuse))
plot(rc_proj[[1:3]])

# function to fix the names of bioclimatic layers
bio_names <- function(x){
  s <- unlist(strsplit(x, "_"))
  p <- stringr::str_pad(s[length(s)], width = 2, pad = 0)
  y <- paste0("bio_", p)
  return(y)
}
# change the names of layers
for(i in 1:nlyr(rc_proj)){
  names(rc_proj)[i] <- bio_names(names(rc_proj)[i])
}
plot(rc_proj[[1:3]])

# write the clipped files
for(i in 1:nlyr(rc_proj)){
  nm <- names(rc_proj)[i]
  name <- paste0("spatial_data/climate_proj/", nm, ".tif")
  terra::writeRaster(rc_proj[[i]], filename = name, overwrite = TRUE)
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
                          overwrite = FALSE)

# read the produced map
agriculture <- rast("spatial_data/agri_50m.tif")
plot(agriculture)

# aggregate to calculate the mean of 50m agri cells in 1km cells
# as a measure of intensity of agriculture in coarser cells
# 17 * 50 = 850 which is roughly equal to the climate data resolution
agri_1k <- terra::aggregate(agriculture, fact = 17, fun = "mean")
plot(agri_1k)


# rad projected climate data
rc_proj <- rast(list.files("spatial_data/climate_proj/", full.names = TRUE))

# resample to get the same size as climate data
agri_resamp <- terra::resample(x = agri_1k, 
                               y = rc_proj[[1]],
                               method = "bilinear")

# crop and mask to match the region
agri_1km <- terra::crop(agri_resamp, rc_proj[[1]]) %>% 
  terra::mask(rc_proj[[1]],
       overwrite = TRUE, 
       filename = "spatial_data/agri_1km.tif")
names(agri_1km) <- "agriculture"

plot(c(rc_proj[[1:3]], agri_1km))


# chemical usage ----------------------------------------------------------
# estimates of pyrethoid usage
load(file = "spatial_data/SP_hires.Rdata")

agri <- rast("spatial_data/agri_1km.tif")

chems <- SP_hires %>% 
  terra::rast() %>% 
  terra::project(x = ., y = agri, align = TRUE) %>% 
  terra::crop(y = agri) %>% 
  terra::extend(y = agri, filename = "spatial_data/pyre_usage.tif")



