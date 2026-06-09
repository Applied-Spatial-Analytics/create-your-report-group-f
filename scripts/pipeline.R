library(sf)
library(sfnetworks)
library(tidygraph)
library(terra)
library(biscale)
library(cowplot)
library(ggplot2)

# 0. Configuration
CITY_NAME        <- "delft"  # Used to automatically label output files
PATH_STREETS_IN  <- "data/vector/delft_raw_streets.gpkg"
PATH_RASTER_IN   <- "data/raster/landsat_delft.tif"
OUTPUT_DIR       <- "data/"


# 1. Load data and pre-process
streets_raw  <- st_read(PATH_STREETS_IN)
raster_raw   <- rast(PATH_RASTER_IN)

raster_temp_c <- (raster_raw * 0.00341802) + 149.0 - 273.15
streets_proj <- st_transform(streets_raw, crs(raster_temp_c))

message("Clipping satellite raster to study area...")
raster_clipped <- crop(raster_temp_c, vect(streets_proj))

# 2. Compute network popularity
streets_clean <- st_cast(st_collection_extract(streets_proj, "LINESTRING"), "LINESTRING")

message("Calculating road network popularity...")
spatial_network <- as_sfnetwork(streets_clean, directed = FALSE) |>
  tidygraph::convert(to_spatial_subdivision) |>
  activate("edges") |>
  mutate(choice_score = centrality_edge_betweenness())

# Convert the network back to a standard spatial dataframe for plotting
streets_analyzed <- st_as_sf(spatial_network, "edges")

# 3. Spatial Overlay and thermal extraction
message("Extracting mean surface temperatures for street segments...")
extracted_temps <- terra::extract(raster_clipped, vect(streets_analyzed), fun = mean, na.rm = TRUE)
streets_analyzed$surface_temp <- extracted_temps[, 2]


# 4. Plot and save: Angular choice
message("Generating Angular Choice visualization...")
choice_plot <- ggplot() +
  geom_sf(data = streets_analyzed, color = "#e2e8f0", size = 0.4) +
  geom_sf(data = streets_analyzed, aes(color = choice_score, size = choice_score), show.legend = "legend") +
  scale_color_viridis_c(option = "plasma", name = "Pedestrian\nFlow Potential", labels = scales::label_comma()) +
  scale_size_continuous(range = c(0.3, 1.8), guide = "none") +
  theme_void() +
  theme(
    panel.background = element_blank(), plot.background = element_blank(),
    legend.background = element_blank(), legend.box.background = element_blank(),
    legend.title = element_text(size = 11, face = "bold", color = "#1a202c"),
    legend.text = element_text(size = 10, color = "#1a202c")
  )

file_choice <- paste0(OUTPUT_DIR, "pedestrian_choice_", CITY_NAME, ".png")
ggplot2::ggsave(filename = file_choice, plot = choice_plot, bg = "transparent", width = 10, height = 8, dpi = 300)

# 5. Plot and save: Surface Temperature
message("Generating Land Surface Temperature visualization...")
raster_df <- as.data.frame(raster_clipped, xy = TRUE, na.rm = TRUE)
colnames(raster_df) <- c("x", "y", "temperature")

thermal_plot <- ggplot() +
  geom_tile(data = raster_df, aes(x = x, y = y, fill = temperature)) +
  scale_fill_viridis_c(option = "inferno", name = "Surface Temp\n(Celsius °C)") +
  theme_void() +
  theme(
    panel.background = element_blank(), plot.background = element_blank(),
    legend.background = element_blank(), legend.box.background = element_blank(),
    legend.title = element_text(size = 11, face = "bold", color = "#1a202c"),
    legend.text = element_text(size = 10, color = "#1a202c")
  ) +
  coord_equal()

file_thermal <- paste0(OUTPUT_DIR, "surface_temp_", CITY_NAME, ".png")
ggplot2::ggsave(filename = file_thermal, plot = thermal_plot, bg = "transparent", width = 10, height = 8, dpi = 300)

# 6. Plot and save: Surface Temperature
message("Calculating 3x3 bivariate quantile matrices...")
bivariate_matrix <- bi_class(streets_analyzed, x = choice_score, y = surface_temp, style = "quantile", dim = 3)

map_canvas <- ggplot() +
  geom_sf(data = bivariate_matrix, aes(color = bi_class), size = 0.5, show.legend = FALSE) +
  bi_scale_color(pal = "DkBlue", dim = 3) +
  theme_void()

final_composite_output <- ggdraw() +
  draw_plot(map_canvas, 0, 0, 1, 1)

file_bivariate <- paste0(OUTPUT_DIR, CITY_NAME, "_final_bivariate_map.png")
ggplot2::ggsave(filename = file_bivariate, plot = final_composite_output, width = 10, height = 8, dpi = 300)

message("Pipeline complete! All files exported successfully to the data directory.")













bivariate_matrix <- bi_class(streets_analyzed, x = choice_score, y = surface_temp, style = "quantile", dim = 3)

map_canvas <- ggplot() +
  geom_sf(data = bivariate_matrix, aes(color = bi_class), size = 0.5, show.legend = FALSE) +
  bi_scale_color(pal = "DkBlue", dim = 3) +
  theme_void()


final_composite_output <- ggdraw() +
  draw_plot(map_canvas, 0, 0, 1, 1)

# 5. Save map as PNG
ggplot2::ggsave(filename = "data/xian_final_bivariate_map.png", plot = final_composite_output, width = 10, height = 8, dpi = 300)

message("Yay done!")


# 6. Angular choice plot and save
isolated_choice_plot <- ggplot() +
  geom_sf(data = streets_analyzed, color = "#e2e8f0", size = 0.4) +
  geom_sf(data = streets_analyzed,
          aes(color = choice_score, size = choice_score),
          show.legend = "legend") +
  scale_color_viridis_c(
    option = "plasma",
    name = "Pedestrian\nFlow Potential",
    labels = scales::label_comma()
  ) +
  scale_size_continuous(range = c(0.3, 1.8), guide = "none") +
  theme_void() +
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.title = element_text(size = 11, face = "bold", color = "#1a202c"),
    legend.text = element_text(size = 10, color = "#1a202c")
  )

# Force ggsave to keep it transparent on export
ggplot2::ggsave(
  filename = "data/pedestrian_choice_xian.png",
  plot = isolated_choice_plot,
  bg = "transparent",
  width = 10,
  height = 8,
  dpi = 300
)

# 7. Thermal map plot and save
# 1. Convert the satellite grid into a standard data frame of X, Y, and Temp coordinates
delft_raster_df <- as.data.frame(raster_clipped, xy = TRUE, na.rm = TRUE)

# Rename the columns so the code is easy to read
# (Assuming your raster's layer name is column 3)
colnames(delft_raster_df) <- c("x", "y", "temperature")

# 2. Plot using standard ggplot tiles
thermal_map_plot <- ggplot() +
  geom_tile(data = delft_raster_df, aes(x = x, y = y, fill = temperature)) +

  # Set a high-contrast thermal scale (Inferno)
  scale_fill_viridis_c(
    option = "inferno",
    name = "Surface Temp\n(Celsius °C)"
  ) +

  # Completely strip out all background canvas blocks
  theme_void() +
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.title = element_text(size = 11, face = "bold", color = "#1a202c"),
    legend.text = element_text(size = 10, color = "#1a202c")
  ) +
  coord_equal() # Forces the map aspect ratio to stay perfectly square

# 3. Export to a true transparent PNG
ggplot2::ggsave(
  filename = "data/surface_temp_xian.png",
  plot = thermal_map_plot,
  bg = "transparent",
  width = 10,
  height = 8,
  dpi = 300
)


# 8. Export .gpkg for Clustering
library(dplyr)

# 1. Cleaning the Attributes Table to only have geometry and computed variables
message("Cleaning GeoData... ")
streets_clean <- streets_analyzed %>%
  select(
    choice_score,
    surface_temp,
    geom
  )

#2. Exporting the Geopackage
message("Exporting GeoPackage...")
st_write(
  streets_clean,
  paste0(OUTPUT_DIR, CITY_NAME, "_street_metrics.gpkg"),
  delete_dsn = TRUE
)
message("GeoPackage exported successfully.Wohoo!")

