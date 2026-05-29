#First install and import packages;
#install only needs to be done once (so you can comment it out after running it the first time)

#install.packages("tidyverse")
#install.packages("sf)
##install.packages("osmdata")

library(tidyverse)
library(sf)
library(osmdata)

#get the data within a bounding box
bbdelft <- getbb("Delft")
bbdelft #to check the min/max coordinates and see if we got the right city

bbxian <- getbb("Xi'an")
bbxian

#if there is a connection error, try this command:
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

# List the features of OSM data
#available_features()

#create OSM objects - DELFT
bd <-opq(bbox = bbdelft, timeout = 10000) |>
  add_osm_feature(key = "building") |>
  osmdata_sf()

hd <-opq(bbox = bbdelft, timeout = 10000) |>
  add_osm_feature(key = "highway") |>
  osmdata_sf()

#create OSM objects - XI'AN
bx <-opq(bbox = bbxian, timeout = 10000) |>
  add_osm_feature(key = "building") |>
  osmdata_sf()

hx <-opq(bbox = bbxian, timeout = 10000) |>
  add_osm_feature(key = "highway") |>
  osmdata_sf()

#explore the data
#x
#str(x$osm_polygon)
#head(x$osm_polygon)
#summary(x$osm_polygon)

#CREATE POLYGONS/LINES + set the correct CRS

#FOR DELFT:

buildings_delft <- bd$osm_polygons |>
  st_transform(crs=28992)

roads_delft_polygons <- hd$osm_polygons |>
  st_transform(crs=28992)

roads_delft_lines <- hd$osm_lines |>
  st_transform(crs=28992)

roads_delft_points <- hd$osm_points |>
  st_transform(crs=28992)

#FOR XI'AN

buildings_xian <- bx$osm_polygons |>
  st_transform(crs=4326)

roads_xian_polygons <- hx$osm_polygons |>
  st_transform(crs=4326)

roads_xian_lines <- hx$osm_lines |>
  st_transform(crs=4326)

roads_xian_points <- hx$osm_points |>
  st_transform(crs=4326)

#PLOT - DELFT

ggplot() +
  geom_sf(data=roads_delft_lines,
          color = "black",
          size = 0.1) +
  geom_sf(data=buildings_delft,
          fill = "grey",
          color = "grey",
          alpha = 0.8) +
  labs(title="test")

#PLOT - XI'AN

ggplot() +
  geom_sf(data=roads_xian_lines,
          color = "black",
          size = 0.1) +
  geom_sf(data=buildings_xian,
          fill = "grey",
          color = "grey",
          alpha = 0.8) +
  labs(title="test_xian")

#TO PLOT WITH BASE MAP (XI'AN):

#install.packages("leaflet")
library(leaflet)
#install.packages("Node.js")

buildings_xian_2 <- buildings_xian |>
  st_transform(crs=4326)

roads_xian_2 <- roads_xian_lines |>
  st_transform(crs=4326)

basemap <- leaflet(buildings_xian_2) |>
  setView(lng=115.82438, lat=38.05917, zoom=20) |>
  addTiles() |>
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = "blue",
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) |>
  addPolylines(data=roads_xian_2,
               color = "red",
               weight = 2,
               opacity = 0.8)

#to see the plot, run this:
basemap
