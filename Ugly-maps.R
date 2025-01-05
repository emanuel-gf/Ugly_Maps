library(tidyverse)
library(sf)
library(terra)
library(tmap)
library(rnaturalearth) ## get many 
library(elevatr) ##elevation data
library(rayshader)
library(arulesViz)
library(ggplot2)
library(ggimage)
library(stars)
library(dplyr)
library(RColorBrewer)
library(tmaptools)
library(magick)
library(osmdata)


#### -- Boundaries 
#austria shapefile boundaries
austria  <- ne_countries(country = "austria", scale="medium")["adm0_dif"]


######## - Elevation 
## another way austria districts
districts<- ne_states(country = "austria",
               returnclass = c("sf")
              )['woe_name']
     

## get elevation data of austria 
elev_austria <- get_elev_raster(austria,z=5, clip="locations")

##Convert to terra object
elev_austria_terra <- terra::rast(elev_austria)

## Create a df from the raster to plot it 
elev_as_df = as.data.frame(elev_austria_terra,xy = TRUE, na.rm = TRUE) 

## Change the name of the column
names(elev_as_df)[3] <- "Elevation"

#### ------------------- Rivers 
## path of hydrorivers:
full_path <- "D:\\Desktop\\R_files\\River_basin\\HydroRIVERS_v10_eu_shp\\HydroRIVERS_v10_eu.shp"

# Build Bbox
austria_bbox <- sf::st_bbox(austria_sf)
print(c('Austria  bbox', austria_bbox))

## Create a WKT bbox polygon based on the bbox object genereted above
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
print(bbox_wkt_test)

### Fetch the rivers data
au_rivers <- sf::st_read(
  full_path,
  wkt_filter = bbox_wkt_test
) |> 
  sf::st_intersection(
    austria_sf  ##clip using country polygon
  )

## check
plot(au_rivers)

## Creates a new df and includes new columns
## Fetch classes for different lengths
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

## Decrease the numbers of classes mantaining its discrete elements
river_length <- river_length %>%
  mutate(length_breaks = cut(
    length_class,
    breaks = c(-Inf, 1, 2, 5, 10, Inf),
    labels = c("0-10", "10-20", "20-50", "50-100", ">100")
  ))

## ---------------------- PLOTS --------------------------------
## ------  PLOT DEM
austria_plot<-  (
    ggplot2::ggplot() +
      geom_raster(data =elev_as_df ,
                  aes(x = x,
                      y = y, 
                      fill=Elevation)
      )+
      geom_sf(data = districts,
              fill = NA,
              color = "grey")+
      theme_void() +
      scale_fill_distiller(palette = "Greys") +
      theme(
        panel.background = element_blank(), # Ensures no background
        legend.position = "none" 
      )
  ) 
## Save
ggplot2::ggsave("D:\\Desktop\\COPERNICUS\\Classes\\Project-python-arcgis-R\\austria.png",
                austria_plot)


### ---------- PLOT RIVER
## Plot2 with length_breaks
plot_gg <- ggplot()+
  geom_sf(
    data = austria_sf,
    fill="darkgreen",
    color="grey",
    alpha = 0.05
  )+
  geom_sf(
    data = river_length,
    aes(
      color = as.factor(length_breaks),
      size = length_breaks
    ) 
  )+
  theme_void()+
  scale_color_brewer(palette="RdGy")+
  scale_size_manual(
    values = c(0.2,0.2,0.2,0.2,0.4,0.4,0.6,1.0,2)
  )+
  theme(
    panel.background = element_blank(), # Ensures no background
    legend.position = "none" 
  )

plot_gg

## save
ggplot2::ggsave("D:\\Desktop\\COPERNICUS\\Classes\\Project-python-arcgis-R\\austria_rivers.png",
                plot_gg)


###### ------------- PLOT WITH MAGICK --------------------------------------------


##### ----- Ostrich DEM -----------------

### Create the map 
# Load the data that are stored in .jpg files
# ----------
## Austria 
austria_jpg <- magick::image_read("D:\\Desktop\\COPERNICUS\\Classes\\Project-python-arcgis-R\\austria.png") 

austria_ <- austria_jpg |>
  image_scale("400x") 

## Ostrich
avestruz <- magick::image_read("D:\\Desktop\\COPERNICUS\\Classes\\Project-python-arcgis-R\\aveztruz1.png")
annotate_avestruz <- magick::image_annotate(avestruz,
                       "Österreich",
                       size=30,
                       gravity="North",
                       color="black")


### ---
## Composite the image
out <- image_composite(annotate_avestruz,
                       austria_,
                       operator = "Over",
                       offset="+60+46",
                       compose_args="50"
                       )
print(out)

#save it

################ --------- OSTRICH RIVERS
#load it back
austria_rivers_jpg <- magick::image_read("D:\\Desktop\\COPERNICUS\\Classes\\Project-python-arcgis-R\\austria_rivers.png") 

#scale
austria_rivers_ <- austria_rivers_jpg |>
  magick::image_scale("400x")


## Ostrich
avestruz <- magick::image_read("D:\\Desktop\\COPERNICUS\\Classes\\Project-python-arcgis-R\\aveztruz1.png")
annotate_avestruz <- magick::image_annotate(avestruz,
                                            "Österreich",
                                            size=30,
                                            gravity="North",
                                            color="black")


### ---
## Composite the image
out_2 <- magick::image_composite(annotate_avestruz,
                       austria_rivers_,
                       operator = "Over",
                       offset="+60+46",
                       compose_args="50"
                      )

out_2









## Create graph
fig <- image_graph(width=212,height=108,res=72) 
ggplot2::ggplot() +
  geom_raster(data =elev_as_df ,
              aes(x = x,
                  y = y, 
                  fill=Elevation)
  )+
  geom_sf(data = districts,
          fill = NA,
          color = "blue")+
  theme_void() +
  scale_fill_distiller(palette = "GnBu") +
  theme(
    panel.background = element_blank(), # Transparent panel background
    plot.background = element_rect(fill='black'),  # Transparent plot background
    legend.position = "none"                                            # Removes the legend
    
  )
dev.off()
magick::image_border(fig,
                     "green")

## Composite the image
out <- image_mosaic(c(annotate_oistrich,fig))
print(out)
