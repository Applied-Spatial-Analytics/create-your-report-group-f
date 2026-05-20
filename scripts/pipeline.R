library(sf)
library(sfnetworks)
library(tidygraph)
library(terra)
library(biscale)
library(cowplot)
library(ggplot2)

# 1. Load data
delft_streets <- st_read("data/vector/delft_raw_streets.gpkg")
landsat_delft <- rast("data/raster/landsat_delft.tif")

delft_temp_c <- (landsat_delft * 0.00341802) + 149.0 - 273.15
delft_streets_proj <- st_transform(delft_streets, crs(delft_temp_c))

message("Clipping satellite raster to study area...")
delft_raster_clipped <- crop(delft_temp_c, vect(delft_streets_proj))

# 2. Compute network popularity
message("Calculating road network popularity...")
spatial_network <- as_sfnetwork(delft_streets_proj, directed = FALSE) |>
  tidygraph::convert(to_spatial_subdivision) |>
  activate("edges") |>
  mutate(choice_score = centrality_edge_betweenness())

delft_streets_analyzed <- st_as_sf(spatial_network, "edges")

# 3. Extract surcafe temperatures on the network
message("Extracting road temperatures from clipped raster...")
extracted_temps <- terra::extract(delft_raster_clipped, vect(delft_streets_analyzed), fun = mean, na.rm = TRUE)
delft_streets_analyzed$surface_temp <- extracted_temps[, 2]

# 4. Generate bivariate layout
bivariate_matrix <- bi_class(delft_streets_analyzed, x = choice_score, y = surface_temp, style = "quantile", dim = 3)

map_canvas <- ggplot() +
  geom_sf(data = bivariate_matrix, aes(color = bi_class), size = 0.5, show.legend = FALSE) +
  bi_scale_color(pal = "DkBlue", dim = 3) +
  theme_void()

legend_canvas <- bi_legend(pal = "DkBlue", dim = 3, xlab = "Higher Popularity ", ylab = "Higher Surface Heat ", size = 7)

final_composite_output <- ggdraw() +
  draw_plot(map_canvas, 0, 0, 1, 1) +
  draw_plot(legend_canvas, 0.02, 0.02, 0.25, 0.25)

# 5. Save map as PNG
ggplot2::ggsave(filename = "data/delft_final_bivariate_map.png", plot = final_composite_output, width = 10, height = 8, dpi = 300)

message("Yay done!")
