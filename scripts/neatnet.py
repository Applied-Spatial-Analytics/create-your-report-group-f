import geopandas as gpd
import neatnet

# 1. Read data
# Update your path
INPUT_PATH = "/Users/moonchaeyeon/Desktop/moon/tudelft/Q4/ARFW0501/data/xian_network.gpkg"
streets_gdf = gpd.read_file(INPUT_PATH)
streets_gdf = streets_gdf.to_crs(epsg=32649)  # Xian: 32649 / Delft: 28992

# 2. Fix basic topology
streets_cleaned = neatnet.fix_topology(streets_gdf)

# 3. Remove dead-end
min_length = 25  # Xian: 25 / Delft: 10
streets_cleaned = streets_cleaned[streets_cleaned.geometry.length > min_length]

# 4. Save the result
# Update your path
OUTPUT_PATH = "/Users/moonchaeyeon/Desktop/moon/tudelft/Q4/ARFW0501/data/xian_network_neat.gpkg"
streets_cleaned.to_file(OUTPUT_PATH, driver="GPKG")
print('Done!')
