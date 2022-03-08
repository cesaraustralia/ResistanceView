library(tidyverse)
library(terra)
library(sf)


# read RLEM data
rlem_data <- read_csv("data/resist_data.csv") %>% 
  dplyr::filter(SPECIES == "Halotydeus destructor") %>% 
  dplyr::select(SPECIES, RESISTANCE, YEAR, REGION, `CHEM.GROUP`, LONG, LAT)
head(rlem_data)
nrow(rlem_data)

# extracting raster values



# modelling




