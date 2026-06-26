library(sf)
library(sfnetworks)
library(tidygraph)
library(terra)
library(biscale)
library(cowplot)
library(ggplot2)
library(dplyr)
library(plotly)
library(here)
library(ggspatial)
library(stringr)

# ========================================================
# 0. Configuration
# ========================================================
CITY_NAME     <- "Xian"   # "Xian" or "Delft"
PROJECTED_CRS <- 32649    # Xian: 32649, Delft: 28992
OUTPUT_DIR    <- "output/01_Report/03_Results"

PATH_STREETS_IN <- here("scripts", "data", paste0(CITY_NAME, "_network_neat.gpkg"))
PATH_COINS_IN   <- here("scripts", "data", paste0(CITY_NAME, "_network_coins.gpkg"))
PATH_RASTER_IN  <- here("scripts", "data", paste0(CITY_NAME, "_landsat.tif"))
PATH_GREEN_IN   <- here("scripts", "data", paste0(CITY_NAME, "_green.geojson"))
PATH_BLUE_IN    <- here("scripts", "data", paste0(CITY_NAME, "_blue.geojson"))


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
# 3. Aggregate Variables to COINS Network
# ========================================================
message("> Aggregating variables to COINS network...")

# Set CRS of COINS network & street ID
coins_net <- st_read(PATH_COINS_IN, layer = "strokes_0", quiet = TRUE) |>
  st_transform(PROJECTED_CRS) |>
  mutate(coins_id = row_number())

# Calculate length of intersection of raw network and COINS network
intersected <- st_intersection(coins_net, streets_analyzed)
intersected$overlap_length <- as.numeric(st_length(intersected))

# Aggregate variables for each COINS segment using a Length-Weighted Mean.
# We group the data by 'coins_id' and apply 'overlap_length' as the weight,
# ensuring that longer overlapping raw segments contribute proportionally more to the final value.
aggregated_coins <- intersected |>
  st_drop_geometry() |>
  group_by(coins_id) |>
  summarise(
    choice_score  = max(choice_score, na.rm = TRUE),
    surface_temp  = weighted.mean(surface_temp, overlap_length, na.rm = TRUE),
    dist_to_green = weighted.mean(dist_to_green, overlap_length, na.rm = TRUE),
    dist_to_blue  = weighted.mean(dist_to_blue, overlap_length, na.rm = TRUE)
  )

# Join tables
streets_coins_analyzed <- coins_net |>
  left_join(aggregated_coins, by = "coins_id")

# Create an output file
st_write(streets_coins_analyzed, here("scripts", "data", paste0(CITY_NAME, "_variables.gpkg")), delete_dsn = TRUE, quiet = TRUE)

# ========================================================
# 4. K-Means Clustering (on Aggregated COINS Network)
# ========================================================
message("Loading aggregated COINS network for clustering...")

# Read CITY_variables.gpkg
streets_analyzed <- st_read(here("scripts", "data", paste0(CITY_NAME, "_variables.gpkg")), quiet = TRUE)

message("Preparing feature matrix for clustering...")

# Extract variables
features <- streets_analyzed |>
  select(choice_score, surface_temp, dist_to_green, dist_to_blue) |>
  st_drop_geometry() |>
  mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) |>
  mutate(choice_score = log1p(choice_score))  # log

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
k <- 4

  # Standardize cluster numbering
  standardize_clusters <- function(kmeans_result){
  centers <- as.data.frame(kmeans_result$centers)
  centers$original_cluster <- seq_len(nrow(centers))

  # Order by the variables that define your interpretation
  centers <-
    centers |>
    dplyr::arrange(
      dplyr::desc(surface_temp),
      dplyr::desc(choice_score)
    )

  centers$new_cluster <- seq_len(nrow(centers))
  centers |>
    dplyr::select(
      original_cluster,
      new_cluster
    )

}

kmeans_result <- kmeans(X_weighted, centers = k, nstart = 20)
cluster_map <- standardize_clusters(kmeans_result)

streets_analyzed$cluster <-

  cluster_map$new_cluster[
    match(
      kmeans_result$cluster,
      cluster_map$original_cluster
    )
  ]

streets_analyzed$cluster <-

  factor(
    streets_analyzed$cluster,
    levels = 1:4
  )

# Final cluster numbering for cross-city comparison
# Since clustering assigns numbers arbitrarily, we just rearrange the cluster orders to match between the
#cities to ensure that the pipeline is reproducible and comparable

if (tolower(CITY_NAME) == "xian") {

  cluster_num <- as.integer(as.character(streets_analyzed$cluster))

  # Swap clusters 2 and 3
  cluster_num[cluster_num == 2] <- 99
  cluster_num[cluster_num == 3] <- 2
  cluster_num[cluster_num == 99] <- 3

  streets_analyzed$cluster <- factor(
    cluster_num,
    levels = 1:4
  )

}

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
             median = ~median(., na.rm = TRUE),
             min  = ~min(., na.rm = TRUE),
             max  = ~max(., na.rm = TRUE)
           ),
           .names = "{.col}_{.fn}")
  ) |>
  mutate(across(where(is.numeric), ~round(., 2)))

print("=== Cluster Profiling Table ===")
print(cluster_summary)

table(streets_analyzed$cluster)

# ========================================================
# 5. Plotting & Exporting
# ========================================================
source("config.R")

map_bbox <- st_bbox(streets_analyzed)

# Output 1. Clustered map (gpkg)
message("> (1) Clustered map (gpkg)")

st_write(streets_analyzed, paste0(OUTPUT_DIR, CITY_NAME, "_clustered.gpkg"), delete_dsn = TRUE, quiet = TRUE)

# Output 2. Clustered map (png)
message("> (2) Clustering result (png)")

cluster_colors <- c("1" = "#C46D5E", "2" = "#62BBC1", "3" = "#7D84B2", "4" = "#E597C0")

cluster_map <- ggplot() +
  geom_sf(data = streets_analyzed, aes(color = cluster), size = 0.6) +
  scale_color_manual(values = cluster_colors, name = "Cluster Profile") +
  coord_report(map_bbox) +
  labs(title = paste("Spatial Distribution of Clusters:", toupper(CITY_NAME)), x = NULL, y = NULL) +
  theme_report()

export_figure(
  cluster_map,
  paste0(CITY_NAME, "_clusters.png")
)

# Output 2-B. Individual Cluster Typology Maps (Separate Files with Transparent Backgrounds)
message("> (2-B) Generating separate transparent typology maps...")

# Create a master baseline network layer (all grey)
background_network <- streets_analyzed |> st_drop_geometry()

# Define clear titles for your final report layout
profile_titles <- c(
  "1" = "Typology A - Function-Driven Hotspots",
  "2" = "Typology B - Underutilized Comfort Spaces",
  "3" = "Typology C - Balanced Activity Spaces",
  "4" = "Typology D - Comfort-Driven Hotspots"
)

# Loop through each cluster and generate a standalone map
for (current_cluster in c("1", "2", "3", "4")) {
  message(paste0("  >> Rendering separate transparent map for Cluster ", current_cluster, "..."))

  # Filter the active layer to contain only the current cluster
  active_cluster_data <- streets_analyzed |>
    filter(cluster == current_cluster)

  # Fetch the specific hex color code assigned to this typology
  active_color <- cluster_colors[current_cluster]

  # Build the isolated map
  isolated_map <- ggplot() +
    # 1. Base Layer: The entire city network drawn in a muted background grey
    geom_sf(data = streets_analyzed, color = "#e2e8f0", size = 0.4) +

    # 2. Top Layer: Only the current cluster elements highlighted in their true tone
    geom_sf(data = active_cluster_data, color = active_color, size = 0.7) +

    # 3. Maintain consistent map extent
    coord_report(map_bbox) +

    # 4. Titles
    labs(
      title = profile_titles[current_cluster],
      subtitle = paste("Highlighted street typology •", toupper(CITY_NAME)),
      x = NULL,
      y = NULL
    ) +

    theme_report()

  # Define output filename
  output_filename <- file.path(
    OUTPUT_DIR,
    paste0(CITY_NAME, "_typology_", current_cluster, ".png")
  )

  # Export
  export_figure(
    plot = isolated_map,
    filename = paste0(CITY_NAME, "_typology_", current_cluster, ".png")
  )
}

message("Separate transparent cluster assets successfully generated and exported!")


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
cluster_colors <- c("1" = "#62BBC1", "2" = "#C46D5E", "3" = "#7D84B2", "4" = "#E597C0")

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
htmlwidgets::saveWidget(
  quat_plot,
  file = file.path(OUTPUT_DIR, paste0(CITY_NAME, "_cluster_tetrahedron.html"))
)

# Output 4. Cluster plot - Parallel Coordinates
message("> (4) Cluster plot - Parallel Coordinates")

# Calculate avg
cluster_means <- streets_analyzed |>
  st_drop_geometry() |>
  group_by(cluster) |>
  summarise(
    choice_score  = mean(log1p(choice_score), na.rm = TRUE), # MATCH: log-transformed means
    surface_temp  = mean(surface_temp, na.rm = TRUE),
    dist_to_green = mean(dist_to_green, na.rm = TRUE),
    dist_to_blue  = mean(dist_to_blue, na.rm = TRUE)
  ) |>
  mutate(cluster_num = as.numeric(as.character(cluster)))

# Set range for each axes and apply log transform to match
raw_data <- st_drop_geometry(streets_analyzed) |>
  mutate(choice_score = log1p(choice_score))

par_coord_plot_avg <- plot_ly(type = 'parcoords',
                              data = cluster_means,
                              line = list(
                                color = ~cluster_num,
                                # Explicitly define hard stops for discrete cluster coloring
                                colorscale = list(
                                  list(0.00, '#62BBC1'), list(0.25, '#62BBC1'),  # Cluster 1
                                  list(0.25, '#C46D5E'), list(0.50, '#C46D5E'),  # Cluster 2
                                  list(0.50, '#7D84B2'), list(0.75, '#7D84B2'),  # Cluster 3
                                  list(0.75, '#E597C0'), list(1.00, '#E597C0')   # Cluster 4
                                ),
                                width = 5
                              ),
                              dimensions = list(
                                list(range = c(min(raw_data$choice_score, na.rm=T), max(raw_data$choice_score, na.rm=T)),
                                     label = 'Avg Popularity (Log)', values = ~choice_score),
                                list(range = c(min(raw_data$surface_temp, na.rm=T), max(raw_data$surface_temp, na.rm=T)),
                                     label = 'Avg Surface Temp (°C)', values = ~surface_temp),
                                list(range = c(min(raw_data$dist_to_green, na.rm=T), max(raw_data$dist_to_green, na.rm=T)),
                                     label = 'Avg Dist to Green (m)', values = ~dist_to_green),
                                list(range = c(min(raw_data$dist_to_blue, na.rm=T), max(raw_data$dist_to_blue, na.rm=T)),
                                     label = 'Avg Dist to Blue (m)', values = ~dist_to_blue)
                              )
) %>%
  layout(
    title = "Cluster Profiles: Average Values",
    margin = list(b = 60) # Adds padding at the bottom so labels do not cut off
  )

par_coord_plot_avg

# Output 5. Cluster statistic - CSV (mean, median, max, min)
message("> (5) Cluster statistic - CSV (mean, median, max, min)")

write.csv(
  cluster_summary,
  file.path(OUTPUT_DIR, paste0(CITY_NAME, "_cluster_statistics.csv")),
  row.names = FALSE
)

# Output 6. Angular choice plot
message("> (6) Network popularity plot")

choice_plot <- ggplot() +
  geom_sf(data = streets_analyzed, color = "#e2e8f0", size = 0.4) +
  geom_sf(data = streets_analyzed, aes(color = choice_score, size = choice_score), show.legend = "legend") +
  scale_color_viridis_c(option = "plasma", name = "Pedestrian\nFlow Potential", labels = scales::label_comma()) +
  scale_size_continuous(range = c(0.3, 1.8), guide = "none") +
  labs(title = paste("Pedestrian Choice:", toupper(CITY_NAME)), x = NULL, y = NULL) +
  coord_report(map_bbox) +
  theme_report()
  export_figure(
    plot = choice_plot,
    filename = paste0(CITY_NAME, "_pedestrian_choice.png")
  )

# Output 7. Bivariate Map (Network popularity vs Temp)
message("> (7) Bivariate Map (Network popularity vs Temp)")

bivariate_matrix <- bi_class(streets_analyzed, x = choice_score, y = surface_temp, style = "quantile", dim = 3)
map_canvas <- ggplot() +
  geom_sf(data = bivariate_matrix, aes(color = bi_class), size = 0.5, show.legend = FALSE) +
  bi_scale_color(pal = "DkBlue", dim = 3) +
  labs(title = paste("Bivariate Map of Temperature & Choice:", toupper(CITY_NAME)), x = NULL, y = NULL) +
  coord_report(map_bbox) +
  theme_report()

bi_legend_plot <- bi_legend(pal = "DkBlue", dim = 3,xlab = "Pedestrian Choice",ylab = "Surface Temperature",size = 8)

final_composite_output <- ggdraw() + draw_plot(map_canvas, x = 0, y = 0, width = 0.83, height = 1) +
  draw_plot(bi_legend_plot, x = 0.84, y = 0.18, width = 0.14, height = 0.24)

export_figure(
  plot = final_composite_output,
  filename = paste0(CITY_NAME, "_bivariate_map.png")
)

# Output 8. Surface Temperature
message("> (8) Surface Temperature")

temperature_plot <- ggplot() +
  geom_sf(data = streets_analyzed, color = "#e2e8f0", size = 0.4) +
  geom_sf(data = streets_analyzed, aes(color = surface_temp, size = surface_temp), show.legend = "legend") +
  scale_colour_distiller(palette = "YlOrRd",direction = 1,limits = c(25, 40),oob = scales::squish,name = "Surface\nTemperature (°C)") +
  scale_size_continuous(range = c(0.3, 1.8), guide = "none") +
  labs(title = paste("Surface Temperature:", toupper(CITY_NAME)), x = NULL, y = NULL) +
  coord_report(map_bbox) +
  theme_report()
  export_figure(
    plot = temperature_plot,
    filename = paste0(CITY_NAME, "_surface_temperature.png")
)

# Output 9. Distance to Green
  message("> (9) Distance to Green")

  distance_green_plot <-ggplot() +

    geom_sf(data = streets_analyzed,colour = "#e2e8f0",linewidth = 0.4) +

    geom_sf(data = streets_analyzed,aes(colour = dist_to_green,linewidth = dist_to_green),
            show.legend = TRUE
    ) +

    scale_colour_distiller(palette = "Greens",direction = -1,name = "Distance to\nGreen") +
    scale_linewidth_continuous(range = c(0.3, 1.8), guide = "none") +
    labs(title = paste("Distance to Green:", toupper(CITY_NAME)),x = NULL, y = NULL) +
    coord_report(map_bbox) +
    theme_report()
    export_figure(
      plot = distance_green_plot,
      filename = paste0(CITY_NAME, "_distance_to_green.png")
    )

# Output 10. Distance to Blue
message("> (10) Distance to Blue")

distance_blue_plot <-ggplot() +

  geom_sf(data = streets_analyzed,colour = "#e2e8f0",linewidth = 0.4) +

  geom_sf(data = streets_analyzed,aes(colour = dist_to_blue,linewidth = dist_to_blue),
    show.legend = TRUE
  ) +

  scale_colour_distiller(palette = "Blues",direction = -1,name = "Distance to\nBlue") +
  scale_linewidth_continuous(range = c(0.3, 1.8), guide = "none") +
  labs(title = paste("Distance to Blue:", toupper(CITY_NAME)),x = NULL, y = NULL) +
  coord_report(map_bbox) +
  theme_report()
  export_figure(
    plot = distance_blue_plot,
    filename = paste0(CITY_NAME, "_distance_to_blue.png")
  )

message("Pipeline complete! All processes done. Yay!")
