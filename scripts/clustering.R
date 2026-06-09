library(sf) # for processing vector data DELETE WHILE MERGING ONLY FOR SELF-CHECK
library(dplyr) # for selecting and transforming data

# Step 0. Configuration
CITY_NAME        <- "xian"  # Used to automatically label output files
OUTPUT_DIR       <- "data/"

# Step 1. Data Preparation
message("Loading GeoPackage...")
grids <- st_read("data/delft_street_metrics.gpkg", quiet = TRUE)

## DELETE WHILE MERGING ONLY FOR SELF-CHECK

# View the first few rows of the data
#head(grids)

# Count how many grid units we have
#nrow(grids)
message("Preparing feature matrix...")
features <- grids |>
  select(choice_score,surface_temp) |>
  st_drop_geometry() # remove geometry column so we just keep a data table

# Step 2. Standardization
message("Standardizing variables...")
X_scaled <- scale(features) # Standardize (mean=0, sd=1)

message("Standardization complete.Yay!")

## DELETE WHILE MERGING ONLY FOR SELF-CHECK
#head(X_scaled) # check if standardization worked

## Step 3: Determine the optimal K
## DELETE WHILE MERGING ONLY FOR SELF-CHECK
## Used to visually determine K during methodology development.
## Remove entire step once final K-selection approach is decided.


# Initialize an empty numeric vector to store inertia values
message("Initializing intertia calculations...")
inertia <- numeric()

# Try k values from 2 to 9
k_values <- 2:9

# Loop through each k value
message("Calculating elbow curve...")
for (k in k_values) {
  km <- kmeans(X_scaled, centers = k, nstart = 20)

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

## Step 4. Run K-Means clustering
message("Starting K-Means Clustering.")
set.seed(0)  # So the clustering result is always the same when re-run. Can be 0,10,345,etc.

# Choose the number of clusters based on the elbow plot
k <- 4

# Run K-means clustering on the standardized data
kmeans_result <- kmeans(X_scaled, centers = k, nstart = 20)

# Add the cluster labels to the spatial data
grids$cluster <- as.factor(kmeans_result$cluster) # The result kmeans_result$cluster is a list of cluster labels (1 to 4), in the same order as the original rows in X_scaled and grids

head(grids) ## DELETE WHILE MERGING ONLY FOR SELF-CHECK

print(table(grids$cluster)) # Show how many grids fall into each cluster for quality control

## Step 5. Plot and save: K-Means Clustering
st_write(
  grids,
  paste0(OUTPUT_DIR, CITY_NAME, "_street_metric_clustered.gpkg"),
  delete_dsn = TRUE
)
message("Clustered GeoPackage exported.")

# Plot clusters with base R
plot(grids["cluster"],
     main = "Spatial Pattern of Urban Heat Clusters",
     border = NA)
message("Clustered plotted.")

## Step 5. Interpret Cluster Centers
# Get the cluster centers (in standardized form)
scaled_centers <- kmeans_result$centers

# Print them
print("Cluster centers (standardized):")
print(scaled_centers)

# Convert the centers back to original scale: x * SD + mean
original_centers <- t(apply(
  scaled_centers, 1,
  function(x) x * attr(X_scaled, "scaled:scale") + attr(X_scaled, "scaled:center")
))

# Print the real-world values
print("Cluster centers (original):")
print(original_centers)

## Step 6. Visualize cluster points in 3D distribution
cluster_colors <- c(
  "1" = "#65C3A1",
  "2" = "#FC9964",
  "3" = "#869DC5",
  "4" = "#E597C0"
)


library(plotly)
cluster2d_scaled <- plot_ly(
  x = X_scaled[, "choice_score"],
  y = X_scaled[, "surface_temp"],
  type = "scatter",
  mode = "markers",
  color = as.factor(kmeans_result$cluster),
  colors = cluster_colors,
  marker = list(size = 4)
) %>%
  layout(
    xaxis = list(title = "Street Popularity"),
    yaxis = list(title = "Surface Temperature")
  )

cluster2d_scaled
