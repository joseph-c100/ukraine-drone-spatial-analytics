#install packages
install.packages("tidyverse")
install.packages("sf")
install.packages("sp")
install.packages("spdep")

library(sf)
library(sp)
library(spdep)

#read acled data - Feb 2022 to Feb 2026
data <- read.csv("source-data/ACLED Data_2026-02-03.csv")

#filter for air/drone attacks
data_filtered <- data %>%
  filter(sub_event_type %in% "Air/drone strike")

#convert to spatial object
data_sf <- st_as_sf(data_filtered, 
                    coords = c("longitude", "latitude"),
                    crs = 4326, remove = FALSE)  # WGS84 coordinate system

#population centres
populated_places <- read_sf("source-data/ne_10m_populated_places/ne_10m_populated_places.shp")
populated_places_ukr <- populated_places %>% filter(ADM0_A3 == "UKR")


# Using UTM zone 36N which covers most of Ukraine
# accurate distance calculations
data_sf_projected <- st_transform(data_sf, crs = 32636)
pop_projected <- st_transform(populated_places_ukr, crs = 32636)

# Calculate distance to nearest population centre for each attack
nearest_distances <- st_distance(data_sf_projected, pop_projected)

# Get the minimum distance for each attack (in meters)
data_sf$dist_to_nearest_pop_km <- as.numeric(apply(nearest_distances, 1, min)) / 1000


# Create spatial weights matrix based on k-nearest neighbors
coords <- st_coordinates(data_sf_projected)
knn_weights <- knn2nb(knearneigh(coords, k = 5))  
knn_weights_list <- nb2listw(knn_weights, style = "W")

# calculate Moran's I for distance to population centres
moran.test(data_sf$dist_to_nearest_pop_km, knn_weights_list)
moran.plot(data_sf$dist_to_nearest_pop_km, knn_weights_list)

