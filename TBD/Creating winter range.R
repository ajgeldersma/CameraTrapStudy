# Creating winter range and sampling sites
# Anna Moeller
# 8/5/2015

# Load packages
library(dplyr)
library(rgdal) # readOGR, spTransform
library(sp) # for SpatialPolygons, GridTopology, spTransform
library(rgeos) # for gIntersection, gBuffer, etc. functions
library(maptools) # for unionSpatialPolygons (dissolve function)
library(raster) # for dealing with GAP raster
library(spsurvey) # for GRTS

# Set working directory
setwd("C:/Users/anna.moeller/Documents")

# Load collar points
collars <- readOGR("Camera Trap Study/GPS Collars/Shapefiles", "idfgelk07152015a")

# Add the GMU and Zone each point is in
source("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Sampling/spatial_manip_fns.R")
collars@data$GMU <- xy2gmu(xy = collars)
collars@data$zone <- xy2zone(xy = collars)

# Make NAs in the GMU "Montana" and make the zone "Montana"
### This works, but be careful in future
collars$GMU[is.na(collars$GMU)] <- "Montana"
collars$zone <- as.character(collars$zone)
collars$zone[collars$GMU == "Montana"] <- "Montana"

# Make GMT POSIXct
collars$GMT <- as.POSIXct(collars$GMT, tz = "GMT")

# Okay, let's explore.
length(unique(collars$serial[collars$zone == "Beaverhead"]))
length(unique(collars$serial[collars$GMU == "30"]))

# Which animals are in GMU 30 during winter and summer?
winter30 <- collars[which(collars$GMU == "30" & collars$GMT < as.POSIXct("2015-04-01 00:00:00", tz = "GMT")), ]
summer30 <- collars[which(collars$GMU == "30" & collars$GMT >= as.POSIXct("2015-04-01 00:00:00", tz = "GMT")), ]

# List the animals that wintered in GMU 30
win30vec <- unique(winter30$serial)

# Look at all the points for the collars that wintered in GMU 30
win30 <- collars[collars$serial %in% win30vec, ]

# Where did the win30 collars go for summer?
w <- win30[win30$GMT < as.POSIXct("2015-03-20 00:00:00"), ]
inbtw <- win30[win30$GMT > as.POSIXct("2015-03-20 00:00:00") & win30$GMT < as.POSIXct("2015-05-15 00:00:00"), ]
s <- win30[win30$GMT > as.POSIXct("2015-05-15 00:00:00"), ]

###################
# Let's make a map
###################
# Load GMU and Zone polygon shapefiles
gmus <- readOGR("GIS Layers/IDFG Game Management Units", "GameManagementUnits")
zones <- readOGR("GIS Layers/IDFG Elk Management Zones", "ElkManagementZones")

# Make a shape for GMU 30
gmu30 <- gmus[gmus$NAME == "30", ]

# # Make map of seasonal ranges for elk that winter  in GMU 30
# plot(w, col = "blue", pch = 16)
# plot(inbtw, add = T, col = "pink", pch = 16)
# plot(s, add = T, col = "red", pch = 16)
# plot(gmus, xlim = c(2500000, 2600000), ylim = c(1480000, 1580000), border = "gray", add = T)
# plot(zones, add = T)
# 
# title(main = "Elk that winter in GMU 30")
# centroids <- gCentroid(gmus, byid = T)
# text(x = coordinates(centroids)[,1], y = coordinates(centroids)[,2], labels = gmus$NAME)
# legend("bottomleft",
#        legend = c("Winter Locations", "Intermediate Locations", "Summer Locations"), 
#        fill = c("blue", "pink", "red"), 
#        cex = .8)

#####################################
# Drawing polygon around winter range
#####################################

# Make a sp data frame with only one column, and give every animal the same serial number
w2 <- w[, 1]
w2$serial <- 1

# Make 95% MCP 
# library(adehabitatHR)
# 
# winrange <- mcp(w2,
#                percent = 95,
#                 unin = "m",
#                 unout = "m2")
# plot(winrange, add = T)
# 
# # Get 70% Kernel
# p <- kernelUD(w2,
#               h = "href")
# ver <- getverticeshr(p, 70)
# plot(gmus, xlim = c(2500000, 2600000), ylim = c(1450000, 1600000))
# plot(w, add = T, col = "blue", pch = 16)
# plot(ver, add = T)

# Draw 2km buffer around winter range points
buff <- gBuffer(w2, 
                byid = F,
                width = 2000,
                joinStyle = "ROUND")

######################
# Rasterize everything
######################
# Bring in GAP raster, crop to extent of GMU 30
gap <- raster("GIS Layers/GAP.Idaho/gaplc_IDTM")
gap2 <- crop(gap, extent(gmu30))

# Move from attribute table to simple values, use VALUE_1
gap3 <- deratify(gap2, att = "VALUE_1", complete = T)

# Get the values for human developed and agricultural areas
human_vals <- gap@data@attributes[[1]]$VALUE_1[grepl("human", gap@data@attributes[[1]]$NVC_CLASS, ignore.case = T)]
ag_vals <- gap@data@attributes[[1]]$VALUE_1[grepl("agricul", gap@data@attributes[[1]]$NVC_CLASS, ignore.case = T)]
bad_vals <- c(as.character(human_vals), as.character(ag_vals))

# Make GAP a raster of good and bad sampling areas
gap3[gap3 %in% bad_vals] <- 0
gap3[gap3 != 0] <- 1

# Extract to find which cells are in GMU 30
gap4 <- extract(gap3, gmu30, cellnumbers = T)
gap4.1 <- gap4[[1]][gap4[[1]][, 2] == 1, ]
l <- gap4.1[, 1]

# Make cells in GMU 30 = 1, 0 otherwise
gap30 <- gap3
gap30[l] <- 6
gap30[gap30 != 6] <- 0
gap30[l] <- 1

# Extract to find which cells are in winter buffer
gap5 <- extract(gap30, buff, cellnumbers = T)
gap5.1 <- gap5[[1]][gap5[[1]][, 2] == 1, ]
l <- gap5.1[, 1]

# Make cells in winter buffer = 1, 0 otherwise
gap_win <- gap30
gap_win[l] <- 6
gap_win[gap_win != 6] <- 0
gap_win[l] <- 1

# then aggregate to 1500m cells
fish <- aggregate(gap_win, 50, fun = mean)

# Select grid cells where 70% of smaller grid cells meet the criteria (not in ag, in GMU 30, in winter buff)
fish2 <- fish
fish2[fish2 >= 0.7] <- 1
fish2[fish2 < 0.7] <- 0

# make raster into spatial polygons, give each cell a unique ID
fishpoly <- rasterToPolygons(fish2, fun = function(x){x == 1})
fishpoly$ID <- seq(1:length(fishpoly$gaplc_IDTM))

# run grts on grid, sample 10, oversample 10
design <- list(None = list(panel = c(Panel = 10), seltype = "Equal", over = 10))
sites <- grts(design = design,
              DesignID = "EQUAL",
              type.frame = "area",
              src.frame = "sp.object",
              sp.object = fishpoly,
              shapefile = F)
#              shapefile = T,
#              prjfilename = "GIS Layers/IDFG Game Management Units/GameManagementUnits",
#              out.shape = "Camera Trap Study/Maps/grts_sample")

# Plot it
plot(fish2)
plot(fishpoly, add = T, border = "black")
plot(gmu30, add = T, border = "red")

color <- rep("xx", nrow(sites@data))
color[sites@data$panel == "Panel"] <- "yellow"
color[sites@data$panel == "OverSamp"] <- "light blue"
points(sites, col = color)

# Make polygon outlines of the chosen sites
sampsites <- fishpoly[fishpoly$ID %in% sites$ID, ]

# Add panel, siteID, and order to sampsites@data
sampsites$panel <- sapply(sampsites$ID, function(x){sites$panel[sites$ID == x]})
sampsites$siteID <- sapply(sampsites$ID, function(x){sites$siteID[sites$ID == x]})
sampsites$order <- substr(sampsites$siteID, 7, 8)

# Create 500m grid, then make it into polygons
thirds <- disaggregate(fish2, fact = 3)
thirdspoly <- rasterToPolygons(thirds, fun = function(x){x == 1})

# Subset 500m grid to the sampled sites
sampthirds <- gIntersection(thirdspoly, sampsites, byid=TRUE)

# returns a SpatialCollections, subset to the SpatialPolygons
sampthirds <- sampthirds@polyobj

# Make 500m grid SpatialPolygons into SpatialPolygonsDataFrame for writeOGR function
df <- data.frame("attr" = rep(NA, length(sampthirds)))
sampthirds <- SpatialPolygonsDataFrame(sampthirds, data.frame(df), match.ID = F)

# Write shapefiles
writeOGR(fishpoly, "Camera Trap Study/GRTS", "sampling area", driver = "ESRI Shapefile")
writeOGR(sampsites, "Camera Trap Study/GRTS", "selected sites", driver = "ESRI Shapefile")
writeOGR(sampthirds, "Camera Trap Study/GRTS", "thirds", driver = "ESRI Shapefile")

writeOGR(w, "Camera Trap Study/GPS Collars/Shapefiles", "w", driver = "ESRI Shapefile")
writeOGR(s, "Camera Trap Study/GPS Collars/Shapefiles", "s", driver = "ESRI Shapefile")
writeOGR(inbtw, "Camera Trap Study/GPS Collars/Shapefiles", "inbtw", driver = "ESRI Shapefile")

################################################################################










# Bring in DEM
dem <- raster("GIS Layers/1.GIS layers from Josh/dem")
dem2 <- crop(dem, extent(gmu30))

# Change low elevations to value 1, 0 otherwise
dem2[dem2 <= 2200] <- 1
dem2[dem2 > 2200] <- 0

# Extract to find which cells are in low elevation
gap6 <- overlay(gap3, dem3, fun = min)
gap6.1 <- gap6[[1]][gap6[[1]][, 2] == 1, ]
l <- gap6.1[, 1]

# Make cells in low elevation = 1, 0 otherwise
gap_low <- gap_win
gap_low[l] <- 6
gap_low[gap_low != 6] <- 0
gap_low[l] <- 1 

# Plot stuff
plot(dem2)
plot(gmu30, add = T, border= "red")
plot(w, pch=16, col="blue", add = T)
plot(buff, add = T, border = "pink")










####################################################################################
# Chop fishnet to the right size
################################
# In ArcMap, make a 1.5 x 1.5 km fishnet with the corners at the extent of GMU 30
# Read it in
fishnet <- readOGR("Camera Trap Study/Maps", "fishnet_15km_extent30")

# Make a shape for GMU 30
gmu30 <- gmus[gmus$NAME == "30", ]

# Make a Private + Mesic layer
  # Bring in mesic, clip it to GMU 30
  mesic <- readOGR("GIS Layers/Mesic Areas", "studyareas_mesic_IDTM")
  mesic30 <- mesic[gmu30, ]

  # dissolve all the mesic polygons into one
  mesic30@data$ID <- 1
  mesic30_diss <- unionSpatialPolygons(mesic30, mesic30@data$ID)

  # Bring in landuse agency, pull out private land
  landuse <- readOGR("GIS Layers/INSIDE Idaho Land Ownership", "RLTY_SMA_PUB_24K_POLY")
  priv <- landuse[landuse@data$AGNCY_NAME == "PRIVATE", ]
  
  # clip to make Private + Mesic
  priv_mesic <- gIntersection(mesic30_diss, priv)

# Bring in GAP raster
gap <- raster("GIS Layers/GAP.Idaho/gaplc_IDTM")
gap2 <- crop(gap, extent(gmu30))

# Move from attribute table to simple values, use VALUE_1
gap3 <- deratify(gap2, att = "VALUE_1", complete = T)

# Get the values for human developed and agricultural areas
human_vals <- gap@data@attributes[[1]]$VALUE_1[grepl("human", gap@data@attributes[[1]]$NVC_CLASS, ignore.case = T)]
ag_vals <- gap@data@attributes[[1]]$VALUE_1[grepl("agricul", gap@data@attributes[[1]]$NVC_CLASS, ignore.case = T)]
bad_vals <- c(as.character(human_vals), as.character(ag_vals))

# Make GAP a raster of good and bad sampling areas
gap3[gap3 %in% bad_vals] <- 0
gap3[gap3 != 0] <- 1

# # Change raster to polygons
# p <- rasterToPolygons(gap3, fun = function(x){x == 1})

# Clip fishnet to GMU 30
fishnet30 <- gIntersection(gmu30, fishnet, byid=TRUE)
plot(fishnet30, col = "pink")

# Clip that fishnet to GAP

# Clip that fishnet to winter buffer
fishnet30_xeric_buff <- gIntersection(fishnet30_xeric, buff, byid=TRUE)
plot(fishnet30_xeric_buff, col = "pink")

# Pick out only cells that have an area of at least 2200000
# Make samp a spdataframe, with attribute of area
areas <- sapply(slot(fishnet30_xeric_buff, "polygons"), slot, "area")
samp <-  SpatialPolygonsDataFrame(fishnet30_xeric_buff, data.frame(area = areas), match.ID = F)
samp2 <- samp[samp@data$area >= 2000000, ]

# Make a plot
plot(gmus, xlim = c(2500000, 2600000), ylim = c(1480000, 1580000))
plot(w, add = T, col = "blue", pch = 16)
plot(fishnet, add = T)
plot(gmu30, add = T, border = "green")
plot(buff, add = T, border = "blue")
plot(fishnet30_3, add = T)
plot(fishnet30_winter, add = T, col = "red")

############
# Run GRTS
############
# Create the design list, sample 10, oversample 5
design <- list(None = list(panel = c(Panel = 10), seltype = "Equal", over = 10))

# Run GRTS
sites <- grts(design = design,
             DesignID = "EQUAL",
             type.frame = "area",
             src.frame = "sp.object",
             sp.object = fishnet30_winter,
             shapefile = F)
#              shapefile = T,
#              prjfilename = "GIS Layers/IDFG Game Management Units/GameManagementUnits",
#              out.shape = "Camera Trap Study/Maps/grts_sample")

# Plot it
points(sites, col = "yellow")

################################################################################

###########################
# Make my own fishnet in R
###########################
# Make base plot
plot(gmus, xlim = c(2500000, 2600000), ylim = c(1480000, 1580000))
plot(w, add = T, col = "blue", pch = 16)

# Make a shape for GMU 30
gmu30 <- gmus[gmus$NAME == "30",]
plot(gmu30, add = T, border = "green")

# Make a 1500x1500m grid 
length <- 1500
xmin <- gmu30@bbox[1,1]
ymin <- gmu30@bbox[2,1]
ncellx <- ceiling((gmu30@bbox[1,2] - gmu30@bbox[1,1])/length)
ncelly <- ceiling((gmu30@bbox[2,2] - gmu30@bbox[2,1])/length)

GT <- GridTopology(c(xmin, ymin), c(length, length), c(ncellx, ncelly))
fishnet <- as(as(SpatialGrid(GT), "SpatialPixels"), "SpatialPolygons")

plot(fishnet, add = T)

# Define all the projections before doing subsetting
proj4string(fishnet) <- CRS("+proj=tmerc +lat_0=42 +lon_0=-114 +k=0.9996 +x_0=2500000 +y_0=1200000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0")
gmu30_prj <- spTransform(gmu30, CRS("+proj=tmerc +lat_0=42 +lon_0=-114 +k=0.9996 +x_0=2500000 +y_0=1200000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0"))
buff_prj <- spTransform(buff, CRS("+proj=tmerc +lat_0=42 +lon_0=-114 +k=0.9996 +x_0=2500000 +y_0=1200000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0"))

# Clip instead of subsetting to get polygons that are fully inside GMU 30
fishnet30 <- gIntersection(gmu30_prj, fishnet, byid=TRUE)
plot(fishnet30, col = "pink")

# Pick out only cells that have an area of at least 2200000
# Make fishnet30 a spdataframe, with attribute of area
areas <- sapply(slot(fishnet30, "polygons"), slot, "area")
fishnet30 <-  SpatialPolygonsDataFrame(fishnet30, data.frame(area = areas), match.ID = F)
fishnet30 <- fishnet30[fishnet30@data$area >= 2200000, ]

plot(gmus, xlim = c(2500000, 2600000), ylim = c(1480000, 1580000))
plot(fishnet30, add = T)

# Plot the winter points buffer
plot(buff_prj, add = T, border = "blue")

# Clip fishnet to the points buffer
fishnet30_winter <- fishnet30[buff_prj,]
plot(fishnet30_winter, add = T, col = "red")

########################
# Make a grid of thirds
########################
length <- 500
xmin <- gmu30@bbox[1,1]
ymin <- gmu30@bbox[2,1]
ncellx <- ceiling((gmu30@bbox[1,2] - gmu30@bbox[1,1])/length)
ncelly <- ceiling((gmu30@bbox[2,2] - gmu30@bbox[2,1])/length)

GT <- GridTopology(c(xmin, ymin), c(length, length), c(ncellx, ncelly))
fishnet_thirds <- as(as(SpatialGrid(GT), "SpatialPixels"), "SpatialPolygons")

# Project it
proj4string(fishnet_thirds) <- CRS("+proj=tmerc +lat_0=42 +lon_0=-114 +k=0.9996 +x_0=2500000 +y_0=1200000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0")

# Subset fishnet_thirds.2
thirds <- fishnet_thirds[fishnet30_winter, ]

#Plot the two together
plot(thirds, border = "pink")
plot(fishnet30_winter, add = T)



###############################################################################

# # Working with spatial polygons and sp dataframes
# > slotNames(gmu30) # Gives the names of things you can find with @
# [1] "data"        "polygons"    "plotOrder"   "bbox"        "proj4string"
# > gmu30@bbox
# min     max
# x 2507816 2559703
# y 1497908 1563695

# can also use function bbox(gmu30)

# str(gmu30) shows you how to subset
