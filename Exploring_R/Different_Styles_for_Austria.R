library(tidyverse)
library(sf)
library(terra)
library(tmap)
library(rnaturalearth) ## get many 
library(elevatr) ##elevation data
library(rayshader)
library(ggplot2)
library(ggimage)
library(stars)
library(dplyr)
library(RColorBrewer)
library(tmaptools)
library(osmdata)
library(giscoR)
library(eurostat)

austria_bb <- ## another way austria districts
  districts<- ne_states(country = "austria",
                        returnclass = c("sf")
                        )['woe_name']

#osmdata::available_tags(feature="water")

salzburg_roads_sf <- getbb(place_name = "Salzburg") %>%
                          opq(timeout = 50) %>%
                          add_osm_feature(key = "highway",
                                          value = c("primary", "secondary")) %>%
                          osmdata_sf()

plot(salzburg_roads_sf$osm_lines['name'])

## roads-  primary and secondary
austria_roads_sf <- getbb(place_name = "Austria") %>%
  opq(timeout = 50) %>%
  add_osm_feature(key = "highway",
                  value = c("primary")) %>%
  osmdata_sf()

## railways 
railway_austria_sf <-  getbb(place_name = "Austria") %>%
                      opq(timeout = 50) %>%
                      add_osm_feature(key = "railway") %>%
                      osmdata_sf()
plot(railway_austria_sf$osm_lines['name'])

# Create the plot object, using the osm_lines element of tucson_major
plot_salzburg <- ggplot() +
  geom_sf(data = salzburg_roads_sf$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = 0.2)

plot_salzburg

plot_austria_roads <- ggplot() +
  geom_sf(data = austria_bb,
          fill = "darkblue",
          color = "black",
          alpha = 0.5) +
  geom_sf(data = railway_austria_sf$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = 0.2)

plot_austria


fill = "lightblue", color = "black", alpha = 0.5
cycleway_Salzburg <- osmdata::osmdata_sp(dat1$osm_lines)

plot(dat1)



#### Demographic data
popdens <- get_eurostat("demo_r_d3dens") %>%
  filter(TIME_PERIOD == "2021-01-01")



# Define the 10 biggest cities in Austria and their approximate coordinates
cities <- data.frame(
  name = c("Vienna", "Graz", "Linz", "Salzburg", "Innsbruck", 
           "Klagenfurt", "Villach", "Wels", "Sankt Pölten", "Dornbirn"),
  bbox = I(list(
    getbb("Vienna"),
    getbb("Graz"),
    getbb("Linz"),
    getbb("Salzburg"),
    getbb("Innsbruck"),
    getbb("Klagenfurt"),
    getbb("Villach"),
    getbb("Wels"),
    getbb("Sankt Pölten"),
    getbb("Dornbirn")
  ))
)

# Function to query railways for a city
get_city_railways <- function(city_bbox) {
  opq(bbox = city_bbox) %>%
    add_osm_feature(key = "railway") %>%
    osmdata_sf() %>%
    `[[`("osm_lines")
}

# Retrieve railway data for each city
cities$railways <- lapply(cities$bbox, get_city_railways)

# Combine all railways into one sf object
all_railways <- do.call(rbind, cities$railways)

# Plot the railway connections for the 10 biggest cities
ggplot() +
  geom_sf(data = all_railways, color = "darkgreen", size = 0.3) +
  labs(
    title = "Railway Connections of the 10 Biggest Cities in Austria",
    subtitle = "Data from OpenStreetMap",
    caption = "Created with osmdata and ggplot2"
  ) +
  theme_minimal()