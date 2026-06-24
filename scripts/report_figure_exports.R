# ==========================================================
# Report Figure Exports
# ==========================================================

# ========================================================
# 0. Configuration
# ========================================================
source("config.R")

CITY_NAME     <- "Xian"   # "Xian" or "Delft"
PROJECTED_CRS <- 32649    # Xian: 32649, Delft: 28992
OUTPUT_DIR <- "output/01_Report/02_Methods"

study_extent <- st_read(
  paste0(
    "../report_files/data/",
    CITY_NAME,
    "_bbox_historic_center_buffered.gpkg"
  ),
  quiet = TRUE
)

library(sf)
library(ggplot2)
library(terra)
library(tidyterra)

# ========================================================
# 1. Export OSM Morphology Maps
# ========================================================

basemap <- load_basemap(CITY_NAME)

map_bbox <- st_bbox(study_extent)

street_morphology <-

  base_map(basemap) +

  labs(
    title = paste("Street Morphology:", toupper(CITY_NAME)),
    subtitle = "Historic City Centre"
  ) +

  coord_report(map_bbox) +

  theme_report()

export_figure(
  street_morphology,
  paste0(CITY_NAME, "_street_morphology.png")
)


basemap <- load_basemap(CITY_NAME)

map_bbox <- st_bbox(study_extent)

street_morphology <-

  base_map(basemap) +

labs(
  title = paste("Street, Blue and Green Network:", toupper(CITY_NAME)),
  subtitle = "OSM Derived Data"
) +

  coord_report(map_bbox) +

  theme_report()

export_figure(
  street_morphology,
  paste0(CITY_NAME, "_osm.png")
)
# ========================================================
# 2. Export LST Maps
# ========================================================

basemap <- load_basemap(CITY_NAME)

map_bbox <- st_bbox(study_extent)

landsat <- rast(file.path(
  "data",
  paste0(CITY_NAME, "_landsat.tif")
))

landsat <-
  (landsat * 0.00341802) + 149.0 - 273.15

bbox_poly <- st_as_sfc(map_bbox)

bbox_poly <- st_buffer(
  st_transform(bbox_poly, PROJECTED_CRS),
  dist = 1000
)

bbox_poly <- st_transform(
  bbox_poly,
  crs(landsat)
)

landsat_crop <-
  crop(
    landsat,
    vect(bbox_poly)
  )

lst_map <-

  ggplot() +

  geom_spatraster(
    data = landsat_crop
  ) +

  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    name = "Surface\nTemperature (°C)"
  ) +

  labs(
    title = paste(
      "Land Surface Temperature:",
      toupper(CITY_NAME)
    ),
    subtitle = "Landsat-derived Surface Temperature:"
  ) +

  coord_sf(
    xlim = st_bbox(bbox_poly)[c("xmin","xmax")],
    ylim = st_bbox(bbox_poly)[c("ymin","ymax")],
    expand = FALSE
  ) +

  theme_report()

export_figure(
  lst_map,
  paste0(CITY_NAME, "_surface_temperature_context.png")
)

# ========================================================
# 3. Study Area Delineation
# ========================================================

# Load polygons
delft_center <- st_read("../report_files/data/delft_bbox_historic_centre.gpkg", quiet = TRUE)

xian_center  <- st_read("../report_files/data/xian_bbox_historic_center.gpkg", quiet = TRUE)

# Select city
if (tolower(CITY_NAME) == "delft"){
  centre <- delft_center

} else {
  centre <- xian_center

}

# Load the same basemap used for the street morphology figure
basemap <- load_basemap(CITY_NAME)

map_bbox <- st_bbox(study_extent)

study_area_map <-

  base_map(basemap) +

  # Historic city centre
  geom_sf(
    data = centre,
    fill = NA,
    colour = "#C46D5E",
    linewidth = 1.5
  ) +

  labs(
    title = paste(
      "Study Area Delieanation:",
      toupper(CITY_NAME)
    ),
    subtitle = ""
  ) +

  coord_report(map_bbox) +

  theme_report()

export_figure(
  study_area_map,
  paste0(CITY_NAME, "_study_area.png")
)

message("Study area figure exported.")

# ========================================================
# 4. Study Area Buffer Delineation
# ========================================================

# Load polygons
delft_center <- st_read("../report_files/data/delft_bbox_historic_centre.gpkg", quiet = TRUE)
delft_buffer <- st_read("../report_files/data/delft_bbox_historic_center_buffered.gpkg", quiet = TRUE)

xian_center  <- st_read("../report_files/data/xian_bbox_historic_center.gpkg", quiet = TRUE)
xian_buffer  <- st_read("../report_files/data/xian_bbox_historic_center_buffered.gpkg", quiet = TRUE)

# Select city
if (tolower(CITY_NAME) == "delft"){
  centre <- delft_center
  buffer <- delft_buffer
} else {
  centre <- xian_center
  buffer <- xian_buffer
}

# Load the same basemap used for the street morphology figure
basemap <- load_basemap(CITY_NAME)

map_bbox <- st_bbox(study_extent)

study_area_map <-

  base_map(basemap) +

  # 500 m buffer
  geom_sf(
    data = buffer,
    fill = NA,
    colour = "#C46D5E",
    linewidth = 1.5,
    linetype = "22"
  ) +

  # Historic city centre
  geom_sf(
    data = centre,
    fill = NA,
    colour = "#C46D5E",
    linewidth = 1.5
  ) +
  labs(
    title = paste(
      "Study Area Delieanation:",
      toupper(CITY_NAME)
    ),
    subtitle = "With 500m Analysis Buffer"
  ) +

  coord_report(map_bbox) +

  theme_report()

export_figure(
  study_area_map,
  paste0(CITY_NAME, "_study_area_with_buffer.png")
)

message("Study area buffer figure exported.")
