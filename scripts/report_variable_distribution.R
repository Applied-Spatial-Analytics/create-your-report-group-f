library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)

# ========================================================
# Visualizing data distribution by cluster
# ========================================================

# 0. Configuration
CITY_NAME <- "Xian"  # delft / xian
INPUT_FILE <- paste0("output/01_Report/03_Results", CITY_NAME, "_clustered.gpkg")

# 1. Load the clustered dataset
message("Loading clustered dataset...")
streets_clustered <- st_read(INPUT_FILE, quiet = TRUE)

# 2. Define cluster colors (matching the existing pipeline)
cluster_colors <- c("1" = "#C46D5E", "2" = "#62BBC1", "3" = "#7D84B2", "4" = "#E597C0")

# 3. Transform data (Convert to long format for easier plotting)
# Apply a log transformation (log1p) to popularity for visualization
plot_data <- streets_clustered |>
  st_drop_geometry() |>
  select(cluster, choice_score, surface_temp, dist_to_green, dist_to_blue) |>
  mutate(choice_score_log = log1p(choice_score)) |>
  select(-choice_score) |>
  rename(
    `1. Popularity (Log Choice Score)` = choice_score_log,
    `2. Surface Temp (°C)` = surface_temp,
    `3. Dist to Green (m)` = dist_to_green,
    `4. Dist to Blue (m)` = dist_to_blue
  ) |>
  pivot_longer(cols = -cluster, names_to = "variable", values_to = "value")

# 4. Create density plots (smooth histograms) by variable
message("Plotting distributions...")
distribution_plot <- ggplot(plot_data, aes(x = value, fill = as.character(cluster))) +
  geom_density(alpha = 0.6, color = "white", linewidth = 0.3) + # Add transparency (alpha) so overlapping areas are visible
  facet_wrap(~ variable, scales = "free", ncol = 2) +           # Arrange graphs in a 2x2 grid by variable
  scale_fill_manual(values = cluster_colors, name = "Cluster") +
  theme_minimal() +
  labs(title = paste("Variable Distribution by Cluster:", toupper(CITY_NAME)),
       x = "Value",
       y = "Density") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(b = 15)),
    strip.text = element_text(face = "bold", size = 11, color = "#2c3e50"), # Style the title of each facet
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.spacing = unit(1.5, "lines") # Spacing between panels
  )

# 5. Output to RStudio Viewer
print(distribution_plot)

# 6. Save png
output_image_path <- paste0("output/01_Report/03_Results/", CITY_NAME, "_cluster_distribution.png")

message("Saving distribution plot as PNG...")

ggsave(
  filename = output_image_path,
  plot = distribution_plot,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)
print(distribution_plot)
message("Image saved successfully at: ", output_image_path)
