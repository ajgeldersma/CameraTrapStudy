# GRTS
# Anna Moeller
# 7/22/2015


############ My workflow in Arc ############################
# Make a layer/shp/class for unit 30
# Create a 1-km2 fishnet over unit 30
# Data management tools/Feature Class/Create Fishnet
# Extent same as GMU_30
# Cell Size Width and Height = 1000 (m)
# Type: Polygon
# Clip the label layer to the shape of GMU_30
# Geoprocessing/Clip
# These are the centers of the grid cells
# Add XY coordinates to the clipped layer
# Data management tools/Features/Add XY coordinates
# Save it as a shp

# This didn't work yet because this is set up for cells with area

# So I tried this:
# Open attribute table for MU30_1km_fishnet_poly
# Add field 
# N_centroid and E_centroid
# type = float
# Right click field -> Calculate Geometry
# Export as shp
#########################################################
# Load Packages
library(spsurvey)

# Set working directory
setwd("C:/Users/anna.moeller/Documents/GIS Layers")

att <- read.dbf("GMU_30_fishnet_15")

# Create the design list
design <- list(None = list(panel = c(Panel = 10), seltype = "Equal", over = 5))

# Run grts
fish <- grts(design = design,
             DesignID = "EQUAL",
             type.frame = "area",
             src.frame = "shapefile",
             in.shape = "GMU_30_fishnet_15",
             att.frame = att,
             shapefile = T,
             prjfilename = "GMU_30_fishnet_15",
             out.shape = "grts_selected_15km")
