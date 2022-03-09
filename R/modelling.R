library(tidyverse)
library(terra)
library(sf)
library(dismo)
library(spatstat.core)

# read raster data
rc_proj <- rast(list.files("spatial_data/climate_proj/", full.names = TRUE))
agri <- rast("spatial_data/agri_1km.tif")
names(rc_proj)

r <- c(rc_proj[[c(1,5,12,19)]], agri)
plot(r)

# read RLEM data
rlem_data <- read_csv("data/resist_data.csv") %>% 
  dplyr::filter(SPECIES == "Halotydeus destructor") %>% 
  dplyr::select(SPECIES, RESISTANCE, YEAR, REGION, `CHEM.GROUP`, LONG, LAT) %>% 
  st_as_sf(coords = c("LONG", "LAT"), crs = 4326) %>%
  st_transform(crs = terra::crs(agri))

rlem_data

plot(agri)
plot(st_geometry(rlem_data), add = TRUE)

# extracting raster values



# modelling




