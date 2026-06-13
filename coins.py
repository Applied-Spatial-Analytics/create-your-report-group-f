import geopandas as gpd
import momepy

# UPDATE THESE FIELDS BEFORE RUNNING:

#path to input file:
input_file = "/Users/moonchaeyeon/Desktop/moon/tudelft/Q4/ARFW0501/data/delft_network_neat.gpkg"

#name of output geopackage file:
output_file = "/Users/moonchaeyeon/Desktop/moon/tudelft/Q4/ARFW0501/data/delft_network_coins.gpkg"

#name to output geopackage layers
layer_segments = 'segments_0'
layer_strokes = 'strokes_0'

# CODE STARTS HERE

streets = gpd.read_file(input_file, layer = input_layer)

streets = streets.explode(index_parts=False)
streets = streets.reset_index(drop=True)

#print number of features (optional):
print(streets.geom_type.value_counts())

#to see the crs of the file:
#print(streets.crs)

coins = momepy.COINS(streets, angle_threshold=0, flow_mode=True)

streets["stroke_group"] = coins.stroke_attribute()

strokes = coins.stroke_gdf()

#save output:
streets.to_file(output_file, layer=layer_segments, driver="GPKG")
strokes.to_file(output_file, layer=layer_strokes, driver="GPKG")
