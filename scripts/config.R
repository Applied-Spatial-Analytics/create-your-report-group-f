# ==========================================================
# Report Configuration
# Thermal Comfort in the Historic Core
# Applied Spatial Analytics
# ==========================================================

# ==========================================================
# Libraries
# ==========================================================

library(ggplot2)
library(sf)
library(terra)
library(tidyterra)
library(ggspatial)
library(grid)

# ==========================================================
# Export Settings
# ==========================================================

FIG_DPI <- 600
FIG_BG  <- "TRANSPARENT"

MAP_WIDTH  <- 11
MAP_HEIGHT <- 8

COMPARISON_WIDTH  <- 10
COMPARISON_HEIGHT <- 5

FRAMEWORK_WIDTH  <- 10
FRAMEWORK_HEIGHT <- 6

GRAPH_WIDTH  <- 7
GRAPH_HEIGHT <- 5

# ==========================================================
# Typography
# ==========================================================

FONT_FAMILY <- "Aptos"

AXIS_SIZE <- 9

# ==========================================================
# Common Colours
# ==========================================================

COL_NETWORK <- "#222222"
COL_BORDER  <- "#555555"
DARK_BLUE <- "#6290C3"
LIGHT_BLUE <- "#99b9d6"
DARK_GREEN <- "#004A2E"
LIGHT_GREEN <- "#d9ead3"


# ==========================================================
# Map Layout Defaults
# ==========================================================

MAP_XPAD <- 250
MAP_YPAD <- 250

MAP_WIDTH  <- 11
MAP_HEIGHT <- 8
MAP_DPI    <- 600
MAP_BG     <- "transparent"


# ==========================================================
# Base Map
# ==========================================================
load_basemap <- function(city_name = CITY_NAME,
                         data_dir = "data") {

  roads <- st_read(
    file.path(data_dir, paste0(city_name, "_network_osm.gpkg")),
    quiet = TRUE
  ) |>
    st_transform(PROJECTED_CRS)

  water <- st_read(
    file.path(data_dir, paste0(city_name, "_blue.geojson")),
    quiet = TRUE
  ) |>
    st_transform(PROJECTED_CRS)

  green <- st_read(
    file.path(data_dir, paste0(city_name, "_green.geojson")),
    quiet = TRUE
  ) |>
    st_transform(PROJECTED_CRS)

  list(
    roads = roads,
    water = water,
    green = green,
    bbox = st_bbox(roads)
  )

}

base_map <- function(basemap){

  ggplot() +

    geom_sf(
      data = basemap$water,
      fill = "#99b9d6",
      colour = NA,
      alpha = 0.7
    ) +

    geom_sf(
      data = basemap$green,
      fill = "#d9ead3",
      colour = NA,
      alpha = 0.7
    ) +

    geom_sf(
      data = basemap$roads,
      colour = "#666666",
      linewidth = 0.20
    )

}
# ==========================================================
# Coordinate Display
# ==========================================================

coord_report <- function(map_bbox, pad = 0.02){

  xpad <- (map_bbox["xmax"] - map_bbox["xmin"]) * pad
  ypad <- (map_bbox["ymax"] - map_bbox["ymin"]) * pad

  coord_sf(
    xlim = c(map_bbox["xmin"] - xpad, map_bbox["xmax"] + xpad),
    ylim = c(map_bbox["ymin"] - ypad, map_bbox["ymax"] + ypad),
    expand = FALSE
  )

}

# ==========================================================
# North Arrow
# ==========================================================

north_arrow_report <- function() {

  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_orienteering(
      text_size = 8
    ),
    pad_x = unit(0.35, "cm"),
    pad_y = unit(1.8, "cm"),
    height = unit(1.1, "cm"),
    width = unit(1.1, "cm")
  )

}


# ==========================================================
# Report Theme
# ==========================================================

theme_report <- function(){

  theme_minimal() +

    theme(

      plot.title = element_text(
        size = 20,
        face = "bold",
        hjust = 0.5,
        margin = margin(b = 15)
      ),
      plot.subtitle = element_text(
        size = 11,
        face = "italic",
        colour = "grey40",
        hjust = 0.5,
        margin = margin(b = 12)
      ),

      axis.title = element_blank(),

      axis.text = element_text(
        size = 10,
        colour = "grey20"
      ),

      panel.grid.major = element_line(
        colour = "grey75",
        linewidth = 0.35,
        linetype = "dashed"
      ),

      panel.grid.minor = element_blank(),

      panel.border = element_blank(),

      axis.line = element_blank(),

      panel.background = element_rect(
        fill = "transparent",
        colour = NA
      ),

      plot.background = element_rect(
        fill = "transparent",
        colour = NA
      ),

      legend.background = element_rect(
        fill = "transparent",
        colour = NA
      ),

      legend.box.background = element_rect(
        fill = "transparent",
        colour = NA
      ),

      legend.position = "right",

      legend.justification = "bottom",

      legend.title = element_text(
        size = 12,
        face = "bold"
      ),

      legend.text = element_text(
        size = 10
      ),

      plot.margin = margin(
        15,
        20,
        15,
        15
      )

    )

}

# ==========================================================
# Figure Export
# ==========================================================

export_figure <- function(plot,
                          filename,
                          output_dir = OUTPUT_DIR){

  ggsave(
    filename = file.path(output_dir, filename),
    plot = plot,
    width = MAP_WIDTH,
    height = MAP_HEIGHT,
    dpi = MAP_DPI,
    bg = MAP_BG
  )

}
