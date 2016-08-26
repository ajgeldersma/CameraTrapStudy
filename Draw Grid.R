# Anna Moeller
# 1/6/2015
################################################################################
library(rgdal)
library(sp)
library(raster)

# Read in GMU shapefile
Idaho <- readOGR("C:/Users/anna.moeller/Documents/GIS Layers/IDFG Game Management Units", "GameManagementUnits")
Idaho$NAME <- as.character(Idaho$NAME)

# Create bounding box around GMU shapefile
bounds <- bbox(Idaho)

# Rasterize data
r <- raster(Idaho, res = 1000)
rid <- rasterize(Idaho[,"NAME"], r)

# Calculate number of gridlines required for every 1000m
xrange <- (bounds[1,2] - bounds[1,1])/1000 
yrange <- (bounds[2,2] - bounds[2,1])/1000

# Plot Idaho raster
plot(rid)

# Draw gridlines every 1000m from minimum value of bounding box
for(i in 1:xrange){
  abline(v = bounds[1,1] + i*1000)
}
for(i in 1:yrange){
  abline(h = bounds[2,1] + i*1000)
}

# Write shapefile - fill in blanks, will this work? 
# writeOGR(xyspdf2, dsn = "F:/IDFG/GPSdeerlocs122013", layer = "idfgdeer", driver = "ESRI Shapefile")

# How to click on cells and save their center coordinates to an object 
#     then plot them as points
# require(raster)
# r <- raster(nrow = 10, ncol = 10)
# r[] <- 1
# centers <- click(r, xy = T)
# points(centers[,c("x","y")], col = "red", pch = 19, cex = 2)

# rIdaho <- raster(bounds, res = 1000, proj = projection(Idaho))
# rIdaho <- rasterize(Idaho, res = 1000)

# How to randomly sample
# cam <- sample(1:ncell(r), 50, replace = F)
# r[] <- 0
# r[cam] <- 1
# samplerandom, samplestratify...

# How to create a raster by drawing a bounding box
# plot(rid)
# e <- drawExtent()
# rbvr <- raster(e, res = 1000, crs = projection(rid))
# rbvr <- resample(rbvr, )

# How to crop only the GMUs I'm interested in
# r[r != "60A"] <- NA
# OR
# r[!r %in% c("60A", "12", "72")] <- NA
# OR
# subs