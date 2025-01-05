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
library(wicket) #handles WKT

path_hydrosheds <- "D:\\R_files\\River_basin\\HydroRIVERS_v10_eu_shp"
full_path <- "D:\\Desktop\\R_files\\River_basin\\HydroRIVERS_v10_eu_shp\\HydroRIVERS_v10_eu.shp"

#Load the Hydrosheds files into R
# %%
shp_file_name <- list.files(
  path = path_hydrosheds,
  pattern = "shp",
  full.names=T
)

shp_file_name
## Austria sf
austria_sf <- ## another way austria districts
  districts<- ne_states(country = "austria",
                        returnclass = c("sf")
  )['woe_name']

plot(austria_sf)
#3box
austria_bbox <- sf::st_bbox(austria_sf)
print(c('Austria  bbox', austria_bbox))

## Create a bbox based on the bbox object genereted above
bbox_wkt_test <- paste("POLYGON(( ",
                       as.character(austria_bbox[1]),
                       " ",
                       as.character(austria_bbox[2]),
                       ",",
                       as.character(austria_bbox[1]),
                       " ",
                       as.character(austria_bbox[4]),
                       ",",
                       as.character(austria_bbox[3]),
                       " ",
                       as.character(austria_bbox[4]),
                       ",",
                       as.character(austria_bbox[3]),
                       " ",
                       as.character(austria_bbox[1]),
                       ",",
                       as.character(austria_bbox[1]),
                       " ",
                       as.character(austria_bbox[2]),
                      "))",
                      sep="")
bbox_wkt_test


### Fetch the rivers data
au_rivers <- sf::st_read(
  full_path,
  wkt_filter = bbox_wkt_test
) |> 
  sf::st_intersection(
    austria_sf  ##clip using country polygon
  )

## Plot to see
plot(au_rivers)


## Investigate
unique(au_rivers$CATCH_SKM)

n_distinct(au_rivers$CATCH_SKM)
distinct(au_rivers$CATCH_SKM)

hist(au_rivers$LENGTH_KM)

au_rivers |>
  filter(LENGTH_KM<=20) |>
  filter(LENGTH_KM>=9) |>
  n_distinct()

##  Filter the data  and create a new column
catchment_area_size <- au_rivers |> 
    dplyr::mutate(
        area_size = as.numeric(CATCH_SKM),
        area_size = dplyr::case_when(
            area_size <= 10 ~ 0,
            area_size <= 20 ~ 10,
            area_size <= 40 ~ 20,
            area_size <= 80 ~ 40,
            area_size <= 100 ~ 80,
            area_size > 100 ~ 100,
            TRUE ~ 0
        )
    ) |> 
    sf::st_as_sf()


river_length <- au_rivers |> 
  dplyr::mutate(
    length_class = as.numeric(LENGTH_KM),
    length_class = dplyr::case_when(
      length_class <= 1 ~ 0,
      length_class <= 1.5 ~ 1.5,
      length_class <= 2 ~ 2,
      length_class <= 3 ~ 3,
      length_class <= 4 ~ 4,
      length_class <= 5 ~ 5,
      length_class <= 6 ~ 6,
      length_class < 9 ~ 8,
      length_class > 9 ~ 10,
      TRUE ~ 0
    )
  ) |> 
  sf::st_as_sf()

plot_gg <- ggplot()+
  geom_sf(
    data = austria_sf,
    fill="darkgreen",
    color="green",
    alpha = 0.05
  )+
  geom_sf(
    data = river_length,
    aes(
      color = as.factor(length_class)
    ) 
  )+
  theme_void()+
  scale_color_brewer("Set1")+
  theme(
    panel.background = element_blank(), # Ensures no background
    legend.position = "none" 
  )

plot_gg


plot(river_length["length_class"],
     )


plot(au_rivers["LENGTH_KM"])