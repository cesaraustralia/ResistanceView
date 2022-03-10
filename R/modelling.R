library(tidyverse)
library(patchwork)
library(terra)
library(sf)
library(dismo)
# library(spatstat.core)


# RLEM resistance data ----------------------------------------------------
# environmental layers
rc_proj <- rast(list.files("spatial_data/climate_proj/", full.names = TRUE))
agri <- rast("spatial_data/agri_1km.tif") %>% 
  setNames("agri")
pyre_sp <- rast("spatial_data/pyre_usage.tif") %>% 
  setNames("selection")
names(rc_proj)

# log transform precipitation layers then combine
r <- rc_proj[[c(18,19)]] %>% 
  # log() %>% 
  # setNames(paste0("log_", names(rc_proj)[c(18,19)])) %>% 
  c(rc_proj[[c(4,8)]], ., agri, pyre_sp)
plot(r)

# read RLEM resistance data
rlem_res <- read_csv("data/resist_data.csv") %>% 
  dplyr::filter(SPECIES == "Halotydeus destructor") %>% 
  dplyr::select(species = SPECIES, year = YEAR, region = REGION, chems = `CHEM.GROUP`, LONG, LAT) %>% 
  filter(chems == "Pyrethroids, Pyrethrins") %>% 
  st_as_sf(coords = c("LONG", "LAT"), crs = 4326) %>%
  st_transform(crs = terra::crs(agri)) %>% 
  mutate(resistance = 1)

rlem_res

plot(r[[1]])
plot(st_geometry(rlem_res), add = TRUE)

# read the RLEM distribution data
rlem_dis <- read.csv("data/h_destructor.csv") %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  st_transform(crs = terra::crs(agri)) %>% 
  mutate(resistance = 0)

# combine the two data
rlem_data <- bind_rows(rlem_res, rlem_dis)

plot(r[[1]])
plot(rlem_data["resistance"], add = TRUE)

# extracting raster values ------------------------------------------------
training <- terra::extract(r, vect(rlem_data)) %>% 
  mutate(resistance  = rlem_data$resistance) %>% 
  dplyr::select(-ID) %>% 
  drop_na("agri")

str(training)
summary(training)
table(training$resistance)

training[which(is.na(training$bio_04)), ]

# plot(r[[1]])
# plot(st_geometry(rlem_data[which(is.na(training$bio_04)), ]), add = TRUE)

# modelling ---------------------------------------------------------------
# calculating the case weights
prNum <- as.numeric(table(training$resistance)["1"])
bgNum <- as.numeric(table(training$resistance)["0"])
wt <- ifelse(training$resistance == 1, 1, prNum / bgNum)
table(wt)

brt <- gbm.step(
  data = training,
  gbm.x = which(names(training) != "resistance"),
  gbm.y = which(names(training) == "resistance"),
  family = "bernoulli", 
  tree.complexity = ifelse(prNum < 50, 1, 5), 
  learning.rate = 0.001,
  bag.fraction = 0.75,
  max.trees = 10000,
  n.trees = 50, 
  n.folds = 5,
  site.weights = wt
)

gbm.plot(brt)

# contributions
varimp <- summary(brt)

ggplot(aes(y = rel.inf, x = reorder(var, rel.inf), fill = rel.inf),
       data = varimp) +
  geom_bar(stat="identity") +
  coord_flip() +
  geom_text(aes(label = paste0(round(rel.inf, 2), "")), color = "gray5", 
            size = 3.5, position = position_nudge(y = + 2)) +
  viridis::scale_fill_viridis(option = "E", direction = -1) +
  labs(y = "Relative influence (%)", x = "Variables") +
  guides(fill = FALSE) +
  theme_classic()

# resample the maps to make predictions faster; for visualization
# for selection pressure 'sum' might makes more sense
rr <- terra::aggregate(r, fact = 5, fun = "mean")

# predict with raster package due to issues with terra::predict
rlem_pred <- raster::predict(object = raster::stack(rr),
                             model = brt,
                             n.trees = brt$gbm.call$best.trees,
                             # cores = 8,
                             progress = "text",
                             filename = "predictions/rlem_resistance.tif",
                             overwrite = TRUE,
                             type = "response")

plot(rlem_pred, main = "RLEM resistance prediction")


# visualisation -----------------------------------------------------------
# read AU states
states <- st_read("spatial_data/aus_states.gpkg") %>% 
  st_crop(xmin = 110, ymin = -45, xmax = 155, ymax = -7)

# project rasters for plotting
reddleg_pred <- rast(rlem_pred) %>% 
  # terra::project(x = ., y = "epsg:4326") %>%
  as.data.frame(xy = TRUE, na.rm = TRUE) %>% 
  setNames(c("x", "y", "resistance"))

# plot the resistance prediction
g1 <- ggplot(data = reddleg_pred, aes(x = x, y = y, fill = resistance)) +
  geom_raster() +
  # viridis::scale_fill_viridis(option = "A", direction = 1) +
  scale_fill_gradientn(colours = terrain.colors(30, rev = TRUE)) +
  geom_sf(data = states, alpha = 0.5, col = "gray70", inherit.aes = FALSE) +
  theme_minimal() +
  coord_sf(crs = st_crs(rlem_data)) +
  labs(x = "Longitude", y = "Latitude", fill = "Resistance \nLikelihood")
g1

# plot the resistance points
g2 <- ggplot(data = reddleg_pred, aes(x = x, y = y)) +
  geom_raster(fill = "gray70") +
  geom_sf(data = rlem_res, alpha = 0.6, col = "blue", inherit.aes = FALSE) +
  theme_minimal() +
  coord_sf(crs = st_crs(rlem_data)) +
  labs(x = "Longitude", y = "Latitude")
g2


g1 + g2



# GPA resistance data -----------------------------------------------------
gpa_res <- read_csv("data/GPA_resistance_database_Aug162021_Roozbeh.csv") %>% 
  filter(COUNTRY == "Australia") %>% 
  drop_na(LATITUDE) %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(crs = terra::crs(agri))

names(gpa_res)
nrow(gpa_res)

plot(gpa_res)

ggplot(data = reddleg_pred, aes(x = x, y = y)) +
  geom_raster(fill = "gray70") +
  geom_sf(data = gpa_res, alpha = 0.6, col = "blue", inherit.aes = FALSE) +
  theme_minimal() +
  coord_sf(crs = st_crs(gpa_res)) +
  labs(x = "Longitude", y = "Latitude")








