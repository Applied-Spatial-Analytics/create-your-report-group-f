library(sf)
library(sfnetworks)
library(tidygraph)
library(terra)
library(biscale)
library(cowplot)
library(ggplot2)
library(dplyr)
library(plotly)

# ========================================================
# 0. Configuration
# ========================================================
CITY_NAME     <- "delft"   # "xian" or "delft"
PROJECTED_CRS <- 28992    # Xian: 32649, Delft: 28992
OUTPUT_DIR    <- "output/"

PATH_STREETS_IN <- paste0("data/", CITY_NAME, "_coins_network.gpkg")
PATH_RASTER_IN  <- paste0("data/", CITY_NAME, "_landsat.tif")
PATH_GREEN_IN   <- paste0("data/", CITY_NAME, "_green.geojson")
PATH_BLUE_IN    <- paste0("data/", CITY_NAME, "_blue.geojson")


# ========================================================
# 1. Load data & preprocess
# ========================================================
message("Loading datasets...")
streets_raw <- st_read(PATH_STREETS_IN, quiet = TRUE)
raster_raw  <- rast(PATH_RASTER_IN)
green_space <- st_read(PATH_GREEN_IN, quiet = TRUE)
blue_space  <- st_read(PATH_BLUE_IN, quiet = TRUE)

# [Vector] CRS conversion
streets_proj <- st_transform(streets_raw, PROJECTED_CRS)
green_space  <- st_transform(green_space, PROJECTED_CRS)
blue_space   <- st_transform(blue_space, PROJECTED_CRS)

# [Raster] Temperature conversion & clipping
raster_temp_c <- (raster_raw * 0.00341802) + 149.0 - 273.15
raster_proj <- project(raster_temp_c, crs(streets_proj))
raster_clipped <- crop(raster_proj, vect(streets_proj))


# ========================================================
# 2. Compute 4 Variables
# ========================================================
# 2-1. Compute network popularity (angular choice)
message("Computing 4 variables...")
message("> (1) Road network popularity")
streets_clean <- st_cast(st_collection_extract(streets_proj, "LINESTRING"), "LINESTRING")

spatial_network <- as_sfnetwork(streets_clean, directed = FALSE) |>
  tidygraph::convert(to_spatial_subdivision) |>
  activate("edges") |>
  mutate(choice_score = centrality_edge_betweenness())

# Append to dataframe for final analysis
streets_analyzed <- st_as_sf(spatial_network, "edges")

# 2-2. Spatial Overlay: Extract Land Surface Temperature
message("> (2) Land surface temperature")
extracted_temps <- terra::extract(raster_clipped, vect(streets_analyzed), fun = mean, na.rm = TRUE)
streets_analyzed$surface_temp <- extracted_temps[, 2]

# 2-3. Distance to Green Space
message("> (3) Distance to green space")
nearest_green_idx <- st_nearest_feature(streets_analyzed, green_space)
streets_analyzed <- streets_analyzed |>
  mutate(
    dist_to_green = as.numeric(st_distance(streets_analyzed, green_space[nearest_green_idx, ], by_element = TRUE))
  )

# 2-4. Distance to Blue Space
message("> (4) Distance to blue space")
nearest_blue_idx <- st_nearest_feature(streets_analyzed, blue_space)
streets_analyzed <- streets_analyzed |>
  mutate(
    dist_to_blue = as.numeric(st_distance(streets_analyzed, blue_space[nearest_blue_idx, ], by_element = TRUE))
  )


# ========================================================
# 3. K-Means Clustering
# ========================================================
message("Preparing feature matrix for clustering...")

# Extract variables
features <- streets_analyzed |>
  select(choice_score, surface_temp, dist_to_green, dist_to_blue) |>
  st_drop_geometry() |>
  mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))


# Standardize variables
X_scaled <- scale(features)

# Configure weights
weights <- c(choice_score = 1.0,
             surface_temp = 2.0,
             dist_to_green = 0.5,
             dist_to_blue = 0.5)

# Reflect weights
X_weighted <- sweep(X_scaled, 2, weights, `*`)

# Determine the optimal K
# Initialize an empty numeric vector to store inertia values
inertia <- numeric()

# Try k values from 2 to 9
k_values <- 2:9

# Loop through each k value
message("Calculating elbow curve...")
for (k in k_values) {
  km <- kmeans(X_weighted, centers = k, nstart = 20)

  # # tot.withinss = Total Within-Cluster Sum of Squares
  # This measures how compact the clusters are: lower is better.
  inertia <- c(inertia, km$tot.withinss)
}

# Combine the results into a data frame for plotting
elbow_df <- data.frame(k = k_values, inertia = inertia)


print(elbow_df)

message("Elbow calculation complete.")

# Make the elbow plot
plot(k_values, inertia,
     type = "b",                  # shown both points + lines
     col = "black",
     main = "Elbow Method")

message("Starting K-Means Clustering...")
set.seed(0)
k <- 3

kmeans_result <- kmeans(X_weighted, centers = k, nstart = 20)
streets_analyzed$cluster <- as.factor(kmeans_result$cluster)

# Print results
message("> Extracting Cluster Summary Statistics...")

cluster_summary <- streets_analyzed |>
  st_drop_geometry() |>
  group_by(cluster) |>
  summarise(
    # Calculate mean, min, max
    across(c(choice_score, surface_temp, dist_to_green, dist_to_blue),
           list(
             mean = ~mean(., na.rm = TRUE),
             min  = ~min(., na.rm = TRUE),
             max  = ~max(., na.rm = TRUE)
           ),
           .names = "{.col}_{.fn}")
  ) |>
  mutate(across(where(is.numeric), ~round(., 2)))

print("=== Cluster Profiling Table ===")
print(cluster_summary)


# ========================================================
# 4. Plotting & Exporting
# ========================================================
# Output 1. Clustered map (gpkg)
message("> (1) Clustered map (gpkg)")

st_write(streets_analyzed, paste0(OUTPUT_DIR, CITY_NAME, "_clustered.gpkg"), delete_dsn = TRUE, quiet = TRUE)

# Output 2. Clustered map (png)
message("> (2) Clustering result (png)")

cluster_colors <- c("1" = "#65C3A1", "2" = "#FC9964", "3" = "#869DC5", "4" = "#E597C0")

cluster_map <- ggplot() +
  geom_sf(data = streets_analyzed, aes(color = cluster), size = 0.6) +
  scale_color_manual(values = cluster_colors, name = "Cluster Profile") +
  theme_void() +
  labs(title = paste("Spatial Distribution of Clusters:", toupper(CITY_NAME))) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 10)))

ggsave(paste0(OUTPUT_DIR, CITY_NAME, "_clustered.png"), cluster_map, width = 10, height = 8, dpi = 300, bg = "white")

# Output 3. Cluster plot - 3D Tetrahedron
message("> (3) Cluster plot - 3D Tetrahedron")

# Normalize variables (0~1)
normalize_minmax <- function(x) {
  (x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE))
}

# Set a dataframe
features_minmax <- features %>%
  mutate(across(everything(), normalize_minmax))

# Convert to composition (proportions) so each row sums to 1 (100%)
features_comp <- features_minmax / rowSums(features_minmax)
features_comp[is.na(features_comp)] <- 0 # Prevent division by zero

# Define the coordinates of the 4 vertices for the 3D Tetrahedron
v1 <- c(1, 1, 1)    # Vertex 1: Popularity
v2 <- c(1, -1, -1)  # Vertex 2: Surface Temp
v3 <- c(-1, 1, -1)  # Vertex 3: Dist to Green
v4 <- c(-1, -1, 1)  # Vertex 4: Dist to Blue

# Map each data point to 3D X, Y, Z coordinates inside the tetrahedron (Barycentric mapping)
x_3d <- features_comp[[1]]*v1[1] + features_comp[[2]]*v2[1] + features_comp[[3]]*v3[1] + features_comp[[4]]*v4[1]
y_3d <- features_comp[[1]]*v1[2] + features_comp[[2]]*v2[2] + features_comp[[3]]*v3[2] + features_comp[[4]]*v4[2]
z_3d <- features_comp[[1]]*v1[3] + features_comp[[2]]*v2[3] + features_comp[[3]]*v3[3] + features_comp[[4]]*v4[3]

plot_data_3d <- data.frame(X = x_3d, Y = y_3d, Z = z_3d, cluster = streets_analyzed$cluster)

# Coordinates for drawing the wireframe edges of the tetrahedron
edge_x <- c(v1[1],v2[1],NA, v1[1],v3[1],NA, v1[1],v4[1],NA, v2[1],v3[1],NA, v3[1],v4[1],NA, v4[1],v2[1])
edge_y <- c(v1[2],v2[2],NA, v1[2],v3[2],NA, v1[2],v4[2],NA, v2[2],v3[2],NA, v3[2],v4[2],NA, v4[2],v2[2])
edge_z <- c(v1[3],v2[3],NA, v1[3],v3[3],NA, v1[3],v4[3],NA, v2[3],v3[3],NA, v3[3],v4[3],NA, v4[3],v2[3])

# Create Plotly 3D plot
cluster_colors <- c("1" = "#65C3A1", "2" = "#FC9964", "3" = "#869DC5", "4" = "#E597C0")

quat_plot <- plot_ly() %>%
  # Add edges (wireframe)
  add_trace(x = edge_x, y = edge_y, z = edge_z, type = 'scatter3d', mode = 'lines',
            line = list(color = 'gray', width = 2), hoverinfo = 'none', showlegend = FALSE) %>%
  # Add labels for each vertex
  add_trace(x = c(v1[1], v2[1], v3[1], v4[1]),
            y = c(v1[2], v2[2], v3[2], v4[2]),
            z = c(v1[3], v2[3], v3[3], v4[3]),
            type = 'scatter3d', mode = 'text',
            text = c("<b>Popularity</b>", "<b>Surface Temp</b>", "<b>Dist to Green</b>", "<b>Dist to Blue</b>"),
            textfont = list(size = 14, color = "black"), hoverinfo = 'none', showlegend = FALSE) %>%
  # Add actual data points mapped to cluster colors
  add_trace(data = plot_data_3d, x = ~X, y = ~Y, z = ~Z,
            type = 'scatter3d', mode = 'markers',
            color = ~cluster, colors = cluster_colors,
            marker = list(size = 4, opacity = 0.8)) %>%
  layout(
    title = paste("Quaternary Cluster Plot:", toupper(CITY_NAME)),
    scene = list(
      xaxis = list(visible = FALSE), # Hide default axes to only show the wireframe
      yaxis = list(visible = FALSE),
      zaxis = list(visible = FALSE),
      camera = list(eye = list(x = 1.5, y = 1.5, z = 1.5))
    )
  )

quat_plot

# [Optional] Save to HTML
htmlwidgets::saveWidget(quat_plot, file = paste0(OUTPUT_DIR, CITY_NAME, "_cluster_tetrahedron.html"))

# Output 4. Cluster plot - Parallel Coordinates
message("> (4) Cluster plot - Parallel Coordinates")

# Calculate avg
cluster_means <- streets_analyzed |>
  st_drop_geometry() |>
  group_by(cluster) |>
  summarise(
    choice_score = mean(choice_score, na.rm = TRUE),
    surface_temp = mean(surface_temp, na.rm = TRUE),
    dist_to_green = mean(dist_to_green, na.rm = TRUE),
    dist_to_blue = mean(dist_to_blue, na.rm = TRUE)
  ) |>
  mutate(cluster_num = as.numeric(as.character(cluster)))

# Set range for each axes
raw_data <- st_drop_geometry(streets_analyzed)

par_coord_plot_avg <- plot_ly(type = 'parcoords',
                              data = cluster_means,
                              line = list(
                                color = ~cluster_num,
                                colorscale = list(c(0, '#65C3A1'), c(0.33, '#FC9964'), c(0.66, '#869DC5'), c(1, '#E597C0')),
                                width = 4
                              ),
                              dimensions = list(
                                list(range = c(min(raw_data$choice_score, na.rm=T), max(raw_data$choice_score, na.rm=T)),
                                     label = 'Avg Popularity', values = ~choice_score),
                                list(range = c(min(raw_data$surface_temp, na.rm=T), max(raw_data$surface_temp, na.rm=T)),
                                     label = 'Avg Surface Temp (°C)', values = ~surface_temp),
                                list(range = c(min(raw_data$dist_to_green, na.rm=T), max(raw_data$dist_to_green, na.rm=T)),
                                     label = 'Avg Dist to Green (m)', values = ~dist_to_green),
                                list(range = c(min(raw_data$dist_to_blue, na.rm=T), max(raw_data$dist_to_blue, na.rm=T)),
                                     label = 'Avg Dist to Blue (m)', values = ~dist_to_blue)
                              )
) %>%
  layout(title = "Cluster Profiles: Average Values")

par_coord_plot_avg

# Output 5. Cluster statistic - CSV (mean, max, min)
message("> (5) Cluster statistic - CSV (mean, max, min)")

write.csv(cluster_summary, paste0(OUTPUT_DIR, CITY_NAME, "_cluster_statistic.csv"), row.names = FALSE)

# Output 6. Angular choice plot
message("> (6) Network popularity plot")

choice_plot <- ggplot() +
  geom_sf(data = streets_analyzed, color = "#e2e8f0", size = 0.4) +
  geom_sf(data = streets_analyzed, aes(color = choice_score, size = choice_score), show.legend = "legend") +
  scale_color_viridis_c(option = "plasma", name = "Pedestrian\nFlow Potential", labels = scales::label_comma()) +
  scale_size_continuous(range = c(0.3, 1.8), guide = "none") +
  theme_void() +
  theme(legend.title = element_text(size = 11, face = "bold"), legend.text = element_text(size = 10))

ggsave(filename = paste0(OUTPUT_DIR, "pedestrian_choice_", CITY_NAME, ".png"), plot = choice_plot, bg = "transparent", width = 10, height = 8, dpi = 300)

# Output 7. Bivariate Map (Network popularity vs Temp)
message("> (7) Bivariate Map (Network popularity vs Temp)")

bivariate_matrix <- bi_class(streets_analyzed, x = choice_score, y = surface_temp, style = "quantile", dim = 3)
map_canvas <- ggplot() +
  geom_sf(data = bivariate_matrix, aes(color = bi_class), size = 0.5, show.legend = FALSE) +
  bi_scale_color(pal = "DkBlue", dim = 3) +
  theme_void()

final_composite_output <- ggdraw() + draw_plot(map_canvas, 0, 0, 1, 1)
ggsave(filename = paste0(OUTPUT_DIR, CITY_NAME, "_bivariate_map.png"), plot = final_composite_output, width = 10, height = 8, dpi = 300)


message("Pipeline complete! All processes done. Yay!")


# ========================================================
# TESTING OUTPUTS (DELETE LATER)
# ========================================================

message("> Cluster Means")

cluster_summary |>
  select(
    cluster,
    choice_score_mean,
    surface_temp_mean,
    dist_to_green_mean,
    dist_to_blue_mean
  ) |>
  print()

message("> Cluster Sizes")

streets_analyzed |>
  st_drop_geometry() |>
  count(cluster) |>
  mutate(
    percent = round(n / sum(n) * 100, 1)
  ) |>
  print()

cluster_summary |>
  select(
    cluster,
    choice_score_min,
    choice_score_max,
    surface_temp_min,
    surface_temp_max,
    )

cluster_summary |>
  select(
    cluster,
    dist_to_green_min,
    dist_to_green_max,
    dist_to_blue_min,
    dist_to_blue_max
  )
