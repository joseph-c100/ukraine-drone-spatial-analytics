#Install packages
install.packages("tidyverse")
install.packages("sf")
install.packages("ggplot2")
install.packages("geojsonsf")
install.packages("dplyr")
install.packages("stringr")

library(sf)
library(ggplot2)
library(geojsonsf)
library(dplyr)
library(stringr)

#read acled data - Feb 2022 to Feb 2026 (drone and shelling)
data <- read.csv("source-data/ACLED Data_2026-02-03.csv")

#filter for air/drone attacks
data_filtered <- data %>%
  filter(sub_event_type %in% "Air/drone strike")



data_sf <- st_as_sf(data_filtered, 
                    coords = c("longitude", "latitude"),
                    crs = 4326, remove = FALSE)  # WGS84 coordinate system

populated_places <- read_sf("source-data/ne_10m_populated_places/ne_10m_populated_places.shp")
populated_places_ukr <- populated_places %>% filter(ADM0_A3 == "UKR")

#Ukraine Admin 1 boundaries geojson
ukr_admn1 <- read_sf("source-data/ukraine-boundaries.geojson")
ukr_admn1 <- ukr_admn1 %>% mutate(shapeName = word(shapeName, 1))

#Read admin1 population data
ukr_admn1_pop <- read.csv("source-data/ukr_admpop_adm1_2022.csv")

# Write to GeoJSON
st_write(data_sf, "source-data/acled_data_filtered.geojson", driver = "GeoJSON", append = TRUE)



# Plot with ggplot2 - all years
p1 <- ggplot(data_sf) +
  geom_sf(aes(color = sub_event_type), size = 2) +  
  theme_minimal() +
  labs(title = "Drone strikes - all years")
ggsave("plots/strikes_all_years.png", p1, width = 10, height = 8)


#Facet by year
p2 <- ggplot(data_sf) +
  geom_sf(aes(color = sub_event_type), size = 0.5) +
  facet_wrap( ~ year) +
  theme_minimal() +
  labs(title = "Strikes")
ggsave("plots/strikes_by_year.png", p2, width = 12, height = 10)






# Count strikes per admin1 region and year
strike_counts <- data_sf %>%
  st_drop_geometry() %>%  
  group_by(admin1, year) %>%
  summarise(count = n(), .groups = "drop")

# Join strike counts and population to the boundaries
ukr_strikes_pop <- ukr_admn1 %>%
  mutate(shapeISO = str_remove(shapeISO, "-")) %>%
  left_join(strike_counts, by = c("shapeName" = "admin1")) %>%
  left_join(ukr_admn1_pop, by = c("shapeISO" = "ADM1_PCODE"))





#spatial statistics
#convert event_date to date format
data_sf$event_date <- as.Date(data_sf$event_date)
hist(data_sf$event_date, freq = TRUE, breaks = "months")

#spatial ellipse
p3 <- ggplot(data_sf) +
  geom_sf(color = "black", size = 1) +
  stat_ellipse(aes(x = longitude, y = latitude, color = factor(year)), 
               type = "norm", level = 0.95, linewidth = 1) +
  theme_minimal() +
  labs(title = "Strikes")
ggsave("plots/strikes_ellipse.png", p3, width = 10, height = 8)




