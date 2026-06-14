import geopandas as gpd
import neatnet

# 1. Read data
# Update your path
INPUT_PATH = "/Users/moonchaeyeon/Desktop/moon/tudelft/Q4/ARFW0501/data/xian_network.gpkg"
streets_gdf = gpd.read_file(INPUT_PATH)
streets_gdf = streets_gdf.to_crs(epsg=32649)  # Xian: 32649 / Delft: 28992

count_before = len(streets_gdf)  # street segment count (pre)

# 2. Fix basic topology
streets_cleaned = neatnet.fix_topology(streets_gdf)

# 3. Remove dead-end
min_length = 25  # Xian: 25 / Delft: 10
streets_cleaned = streets_cleaned[streets_cleaned.geometry.length > min_length]

count_after = len(streets_cleaned)   # street segment count (post)
count_diff = count_before - count_after

# 4. Save the result
# Update your path
OUTPUT_PATH = "/Users/moonchaeyeon/Desktop/moon/tudelft/Q4/ARFW0501/data/xian_network_neat.gpkg"
streets_cleaned.to_file(OUTPUT_PATH, driver="GPKG")
print('Done!')

# Print result
print(f"Street segment count")
print(f"Before: {count_before:,}")
print(f"After: {count_after:,}")
print(f"Removed: {count_diff:,}")
