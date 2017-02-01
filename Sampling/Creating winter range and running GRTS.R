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
  
  ###########################################
  # Unit 30 ###
  ###########################################
  
  # Set working directory
  setwd("C:/Users/anna.moeller/Documents")
  
  # Load collar points
  collars <- readOGR("Camera Trap Study/GPS Collars/Shapefiles", "idfgelk07152015a")
  
  # Add the GMU and Zone each point is in
  source("C:/Users/anna.moeller/Documents/GitHub/CameraTrapStudy/Sampling/spatial_manip_fns.R")
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
  
  # Make map of seasonal ranges for elk that winter  in GMU 30
  plot(w, col = "blue", pch = 16)
  plot(inbtw, add = T, col = "pink", pch = 16)
  plot(s, add = T, col = "red", pch = 16)
  plot(gmus, xlim = c(2500000, 2600000), ylim = c(1480000, 1580000), border = "gray", add = T)
  plot(zones, add = T)
  
  title(main = "Elk that winter in GMU 30")
  centroids <- gCentroid(gmus, byid = T)
  text(x = coordinates(centroids)[,1], y = coordinates(centroids)[,2], labels = gmus$NAME)
  legend("bottomleft",
         legend = c("Winter Locations", "Intermediate Locations", "Summer Locations"), 
         fill = c("blue", "pink", "red"), 
         cex = .8)
  
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
  
  ##############################################################
  # Kellogg
  # More conservative appraoch 11/13/2015
  ####################################### 
  
  # Set working directory
  setwd("C:/Users/anna.moeller/Documents")
  
  # Make a shape for unit 4
  gmus <- readOGR("GIS Layers/IDFG Game Management Units", "GameManagementUnits")
  gmu4 <- gmus[gmus$NAME == "4", ]
  
  # Read in RSF, define projection, crop to gmu4
  rsf <- raster("GIS Layers/Jon Horne/Elk Winter Range RSF.asc")
  proj4string(rsf) <- CRS("+proj=tmerc +lat_0=42 +lon_0=-114 +k=0.9996 +x_0=2500000 +y_0=1200000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0")
  rsf2 <- crop(rsf, extent(gmu4))
  
  # Aggregate
  rsf3 <- aggregate(rsf2, 50, fun = mean)
  
  # Select grid cells where the mean RSF value is at least 0.4
  rsf3[rsf3 >= 0.4] <- 1
  rsf3[rsf3 < 0.4] <- 0
  
  # Bring in GAP raster, crop to extent of rsf2
  ### I am using resample here which isn't the best ####
  ### I tried bilinear but nearest neighbor was way better on keeping the 0s
  gap <- raster("GIS Layers/GAP.Idaho/gaplc_IDTM")
  # gap2 <- resample(gap, rsf2, method = "bilinear") 
  # gap2 <- crop(gap2, extent(rsf2))
  #tst1 <- resample(gap, rsf2, method = "ngb")
  #saveRDS(tst1, file = "Camera Trap Study/GRTS/gap_output_panh.RData")
  gap2 <- readRDS("Camera Trap Study/GRTS/gap_output_panh.RData")
  
  # Get the values for human developed and agricultural areas
  develop_vals <- gap@data@attributes[[1]]$VALUE_1[grepl("develop", gap@data@attributes[[1]]$NVC_CLASS, ignore.case = T)]
  ag_vals <- gap@data@attributes[[1]]$VALUE_1[grepl("agricul", gap@data@attributes[[1]]$NVC_CLASS, ignore.case = T)]
  water_vals <- gap@data@attributes[[1]]$VALUE_1[grepl("open water", gap@data@attributes[[1]]$NVC_CLASS, ignore.case = T)]
  quarry_vals <- gap@data@attributes[[1]]$VALUE_1[grepl("quarries", gap@data@attributes[[1]]$NVC_CLASS, ignore.case = T)]
  # quarries, mines, gravel pits, and oil wells
  bad_vals <- c(as.character(develop_vals), as.character(ag_vals), as.character(water_vals), as.character(quarry_vals))
  
  # Make GAP a raster of good and bad sampling areas
  gap3 <- gap2
  gap3[gap3 %in% bad_vals] <- 0
  gap3[gap3 != 0] <- 1
  
  # Aggregate gap 
  gap4 <- aggregate(gap3, 50, fun = min)
  
  # Combine gap4 and rsf3, return only where they're both 1
  winter <- overlay(gap4, rsf3, fun = min)
  
  # Clip this to Unit 4
  winter4 <- mask(winter, gmu4)
  
  # Clip out Indian Reservation
  cdares <- readOGR("GIS Layers/Panhandle Features", "CDA_Indian_Reservation")
  winter4 <- mask(winter4, cdares, inverse = T)
  
  # Make raster into spatial polygons, give each cell a unique ID
  winterpoly <- rasterToPolygons(winter4, fun = function(x){x == 1})
  winterpoly$ID <- seq(1:dim(winterpoly)[1])
  
  # run grts on grid, sample 10, oversample 10
  design <- list(None = list(panel = c(Panel = 10), seltype = "Equal", over = 10))
  sites <- grts(design = design,
                DesignID = "EQUAL",
                type.frame = "area",
                src.frame = "sp.object",
                sp.object = winterpoly,
                shapefile = F)
  #              shapefile = T,
  #              prjfilename = "GIS Layers/IDFG Game Management Units/GameManagementUnits",
  #              out.shape = "Camera Trap Study/Maps/grts_sample_panh")
  
  # Pick out the grid cells that were selected
  sampsites <- winterpoly[winterpoly$ID %in% sites$ID, ]
  
  # Add attributes to sampsites@data
  sampsites$panel <- sapply(sampsites$ID, function(x){sites$panel[sites$ID == x]})
  sampsites$siteID <- sapply(sampsites$ID, function(x){sites$siteID[sites$ID == x]})
  sampsites$order <- substr(sampsites$siteID, 7, 8)
  
  # Plot the selected sites
  plot(winterpoly)
  color <- rep("xx", nrow(sampsites@data))
  color[sampsites@data$panel == "Panel"] <- "yellow"
  color[sampsites@data$panel == "OverSamp"] <- "light blue"
  plot(sampsites, add = T, col = color)
  
  # Create 500m grid, then make it into polygons
  thirds <- disaggregate(winter2, fact = 3)
  thirdspoly <- rasterToPolygons(thirds, fun = function(x){x == 1})
  
  # Subset 500m grid to the sampled sites
  sampthirds <- gIntersection(thirdspoly, sampsites, byid=TRUE)
  
  # returns a SpatialCollections, subset to the SpatialPolygons
  sampthirds <- sampthirds@polyobj
  
  # Make 500m grid SpatialPolygons into SpatialPolygonsDataFrame for writeOGR function
  df <- data.frame("attr" = rep(NA, length(sampthirds)))
  sampthirds <- SpatialPolygonsDataFrame(sampthirds, data.frame(df), match.ID = F)
  
  # Write shapefiles
  writeOGR(winterpoly, "Camera Trap Study/GRTS", "panh sampling area2", driver = "ESRI Shapefile")
  writeOGR(sampsites, "Camera Trap Study/GRTS", "panh selected sites2", driver = "ESRI Shapefile")
  writeOGR(sampthirds, "Camera Trap Study/GRTS", "panh thirds2", driver = "ESRI Shapefile")
  #################################################################
  
  ##############################################################
  # St. Joe 11/20/2015
  #######################
  
  # Set working directory
  setwd("C:/Users/anna.moeller/Documents")
  
  # Make a shape for unit 6/7
  gmus <- readOGR("GIS Layers/IDFG Game Management Units", "GameManagementUnits")
  # gmu67 <- gUnion(gmus[gmus$NAME == "6", ], gmus[gmus$NAME == "7", ])
  gmu6 <- gmus[gmus$NAME == "6", ]
  
  # Read in RSF, define projection, crop to gmu4
  rsf <- raster("GIS Layers/Jon Horne/Elk Winter Range RSF.asc")
  proj4string(rsf) <- CRS("+proj=tmerc +lat_0=42 +lon_0=-114 +k=0.9996 +x_0=2500000 +y_0=1200000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0")
  rsf2 <- crop(rsf, extent(gmu6))
  
  # Aggregate
  rsf3 <- aggregate(rsf2, 50, fun = mean)
  
  # Select grid cells where the mean RSF value is at least 0.4
  rsf3[rsf3 >= 0.4] <- 1
  rsf3[rsf3 < 0.4] <- 0
  
  # Bring in GAP raster, crop to extent of rsf2
  ### I am using resample here which isn't the best ####
  ### I tried bilinear but nearest neighbor was way better on keeping the 0s
  gap <- raster("GIS Layers/GAP.Idaho/gaplc_IDTM")
  # tst1 <- resample(gap, rsf2, method = "ngb")
  # saveRDS(tst1, file = "Camera Trap Study/GRTS/gap_output_gmu67.RData")
  gap2 <- readRDS("Camera Trap Study/GRTS/gap_output_gmu6.RData")
  
  # Get the values for human developed and agricultural areas
  develop_vals <- gap@data@attributes[[1]]$VALUE_1[grepl("develop", gap@data@attributes[[1]]$NVC_CLASS, ignore.case = T)]
  water_vals <- gap@data@attributes[[1]]$VALUE_1[grepl("open water", gap@data@attributes[[1]]$NVC_CLASS, ignore.case = T)]
  quarry_vals <- gap@data@attributes[[1]]$VALUE_1[grepl("quarries", gap@data@attributes[[1]]$NVC_CLASS, ignore.case = T)]
  # quarries, mines, gravel pits, and oil wells
  bad_vals <- c( as.character(quarry_vals))
  
  # Make GAP a raster of good and bad sampling areas
  gap3 <- gap2
  gap3[gap3 %in% bad_vals] <- 0
  gap3[gap3 != 0] <- 1
  
  # Aggregate gap 
  gap4 <- aggregate(gap3, 50, fun = min)
  
  # Combine gap4 and rsf3, if the gap3 is 0, return 0, otherwise return rsf value
  winter <- overlay(gap4, rsf3, fun = min)
  
  # Clip this to Unit 6
  winter6 <- mask(winter, gmu6)
  
  # Bring in St. Joe Polygon
  stjoe <- readOGR("GIS Layers/Jon Horne", "StJoe_Poly_Projected_Take2")
  # Pick out the one big polygon
  stjoe <- stjoe[1, ]
  
  # Mask winter6 to the St. Joe polygon
  win2 <- mask(winter6, stjoe)
  
  # Make raster into spatial polygons, give each cell a unique ID
  winterpoly <- rasterToPolygons(win2, fun = function(x){x == 1})
  winterpoly$ID <- seq(1:dim(winterpoly)[1])
  
  # run grts on grid, sample 10, oversample 10
  design <- list(None = list(panel = c(Panel = 10), seltype = "Equal", over = 10))
  sites <- grts(design = design,
                DesignID = "EQUAL",
                type.frame = "area",
                src.frame = "sp.object",
                sp.object = winterpoly,
                shapefile = F)
  #              shapefile = T,
  #              prjfilename = "GIS Layers/IDFG Game Management Units/GameManagementUnits",
  #              out.shape = "Camera Trap Study/Maps/grts_sample_panh")
  
  # Pick out the grid cells that were selected
  sampsites <- winterpoly[winterpoly$ID %in% sites$ID, ]
  
  # Add attributes to sampsites@data
  sampsites$panel <- sapply(sampsites$ID, function(x){sites$panel[sites$ID == x]})
  sampsites$siteID <- sapply(sampsites$ID, function(x){sites$siteID[sites$ID == x]})
  sampsites$order <- substr(sampsites$siteID, 7, 8)
  
  # Plot the selected sites
  plot(winterpoly)
  color <- rep("xx", nrow(sampsites@data))
  color[sampsites@data$panel == "Panel"] <- "yellow"
  color[sampsites@data$panel == "OverSamp"] <- "light blue"
  plot(sampsites, add = T, col = color)
  
  # Create 500m grid, then make it into polygons
  thirds <- disaggregate(win2, fact = 3)
  thirdspoly <- rasterToPolygons(thirds, fun = function(x){x == 1})
  
  # Subset 500m grid to the sampled sites
  sampthirds <- gIntersection(thirdspoly, sampsites, byid=TRUE)
  
  # returns a SpatialCollections, subset to the SpatialPolygons
  sampthirds <- sampthirds@polyobj
  
  # Make 500m grid SpatialPolygons into SpatialPolygonsDataFrame for writeOGR function
  df <- data.frame("attr" = rep(NA, length(sampthirds)))
  sampthirds <- SpatialPolygonsDataFrame(sampthirds, data.frame(df), match.ID = F)
  
  # Write shapefiles
  writeOGR(winterpoly, "Camera Trap Study/GRTS", "stjoe sampling area2", driver = "ESRI Shapefile")
  writeOGR(sampsites, "Camera Trap Study/GRTS", "stjoe selected sites2", driver = "ESRI Shapefile")
  writeOGR(sampthirds, "Camera Trap Study/GRTS", "stjoe thirds2", driver = "ESRI Shapefile")
  #################################################################
  #############################################################
  # Other stuff I ended up not using 
  # 
  # # Take out the Silver Mountain Ski Area
  # ski <- readOGR("GIS Layers/Panhandle Features", "SilverMtnSki")
  # # Find the raster cells the hwy passes through
  # tbd <- extract(winter2, ski, cellnumbers = T)
  # # Set the cells on the ski area = 0
  # winter2[tbd[[1]][, 1]] <- 0
  
  # # Bring in DEM
  # dem <- raster("GIS Layers/1.GIS layers from Josh/dem")
  # dem2 <- crop(dem, extent(gmu30))
  # 
  # # Change low elevations to value 1, 0 otherwise
  # dem2[dem2 <= 2200] <- 1
  # dem2[dem2 > 2200] <- 0
  # 
  # # Extract to find which cells are in low elevation
  # gap6 <- overlay(gap3, dem3, fun = min)
  # gap6.1 <- gap6[[1]][gap6[[1]][, 2] == 1, ]
  # l <- gap6.1[, 1]
  # 
  # # Make cells in low elevation = 1, 0 otherwise
  # gap_low <- gap_win
  # gap_low[l] <- 6
  # gap_low[gap_low != 6] <- 0
  # gap_low[l] <- 1 
  # 
  # ################################
  # # Chop fishnet to the right size
  # ################################
  # # In ArcMap, make a 1.5 x 1.5 km fishnet with the corners at the extent of GMU 30
  # # Read it in
  # fishnet <- readOGR("Camera Trap Study/Maps", "fishnet_15km_extent30")
  # 
  # # Make a shape for GMU 30
  # gmu30 <- gmus[gmus$NAME == "30", ]
  # 
  # # Make a Private + Mesic layer
  # # Bring in mesic, clip it to GMU 30
  # mesic <- readOGR("GIS Layers/Mesic Areas", "studyareas_mesic_IDTM")
  # mesic30 <- mesic[gmu30, ]
  #
  # # dissolve all the mesic polygons into one
  # mesic30@data$ID <- 1
  # mesic30_diss <- unionSpatialPolygons(mesic30, mesic30@data$ID)
  # 
  # # Bring in landuse agency, pull out private land
  # landuse <- readOGR("GIS Layers/INSIDE Idaho Land Ownership", "RLTY_SMA_PUB_24K_POLY")
  # priv <- landuse[landuse@data$AGNCY_NAME == "PRIVATE", ]
  # 
  # # clip to make Private + Mesic
  # priv_mesic <- gIntersection(mesic30_diss, priv)
  # 
  # # Clip that fishnet to winter buffer
  # fishnet30_xeric_buff <- gIntersection(fishnet30_xeric, buff, byid=TRUE)
  # plot(fishnet30_xeric_buff, col = "pink")
  # 
  # # Pick out only cells that have an area of at least 2200000
  # # Make samp a spdataframe, with attribute of area
  # areas <- sapply(slot(fishnet30_xeric_buff, "polygons"), slot, "area")
  # samp <-  SpatialPolygonsDataFrame(fishnet30_xeric_buff, data.frame(area = areas), match.ID = F)
  # samp2 <- samp[samp@data$area >= 2000000, ]
  # 
  # ###########################
  # # Make my own fishnet in R
  # ###########################
  # 
  # # Make a 1500x1500m grid 
  # length <- 1500
  # xmin <- gmu30@bbox[1,1]
  # ymin <- gmu30@bbox[2,1]
  # ncellx <- ceiling((gmu30@bbox[1,2] - gmu30@bbox[1,1])/length)
  # ncelly <- ceiling((gmu30@bbox[2,2] - gmu30@bbox[2,1])/length)
  # 
  # GT <- GridTopology(c(xmin, ymin), c(length, length), c(ncellx, ncelly))
  # fishnet <- as(as(SpatialGrid(GT), "SpatialPixels"), "SpatialPolygons")
  # 
  # plot(fishnet, add = T)
  # 
  # # Define all the projections before doing subsetting
  # proj4string(fishnet) <- CRS("+proj=tmerc +lat_0=42 +lon_0=-114 +k=0.9996 +x_0=2500000 +y_0=1200000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0")
  # gmu30_prj <- spTransform(gmu30, CRS("+proj=tmerc +lat_0=42 +lon_0=-114 +k=0.9996 +x_0=2500000 +y_0=1200000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0"))
  # buff_prj <- spTransform(buff, CRS("+proj=tmerc +lat_0=42 +lon_0=-114 +k=0.9996 +x_0=2500000 +y_0=1200000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0"))
  # 
  # # Clip instead of subsetting to get polygons that are fully inside GMU 30
  # fishnet30 <- gIntersection(gmu30_prj, fishnet, byid=TRUE)
  # plot(fishnet30, col = "pink")
  #
  # ########################
  # # Make a grid of thirds
  # ########################
  # length <- 500
  # xmin <- gmu30@bbox[1,1]
  # ymin <- gmu30@bbox[2,1]
  # ncellx <- ceiling((gmu30@bbox[1,2] - gmu30@bbox[1,1])/length)
  # ncelly <- ceiling((gmu30@bbox[2,2] - gmu30@bbox[2,1])/length)
  # 
  # GT <- GridTopology(c(xmin, ymin), c(length, length), c(ncellx, ncelly))
  # fishnet_thirds <- as(as(SpatialGrid(GT), "SpatialPixels"), "SpatialPolygons")
  # 
  # # Project it
  # proj4string(fishnet_thirds) <- CRS("+proj=tmerc +lat_0=42 +lon_0=-114 +k=0.9996 +x_0=2500000 +y_0=1200000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0")
  # 
  # # Subset fishnet_thirds.2
  # thirds <- fishnet_thirds[fishnet30_winter, ]
  
  # ##############
  # # Kellogg original approach
  # ##############
  # 
  # # Set working directory
  # setwd("C:/Users/anna.moeller/Documents")
  # 
  # # Make a shape for unit 4
  # gmus <- readOGR("GIS Layers/IDFG Game Management Units", "GameManagementUnits")
  # gmu4 <- gmus[gmus$NAME == "4", ]
  # 
  # # Read in RSF, define projection, crop to gmu4
  # rsf <- raster("GIS Layers/Jon Horne/Elk Winter Range RSF.asc")
  # proj4string(rsf) <- CRS("+proj=tmerc +lat_0=42 +lon_0=-114 +k=0.9996 +x_0=2500000 +y_0=1200000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0")
  # rsf2 <- crop(rsf, extent(gmu4))
  # 
  # # Bring in GAP raster, crop to extent of rsf2
  # ### I am using resample here which isn't the best ####
  # ### I tried bilinear but nearest neighbor was way better on keeping the 0s
  # gap <- raster("GIS Layers/GAP.Idaho/gaplc_IDTM")
  # # gap2 <- resample(gap, rsf2, method = "bilinear") 
  # # gap2 <- crop(gap2, extent(rsf2))
  # #tst1 <- resample(gap, rsf2, method = "ngb")
  # #saveRDS(tst1, file = "Camera Trap Study/GRTS/gap_output_panh.RData")
  # gap2 <- readRDS("Camera Trap Study/GRTS/gap_output_panh.RData")
  # 
  # # Move from attribute table to simple values, use VALUE_1
  # #gap3 <- deratify(gap2, att = "VALUE_1", complete = T)
  # 
  # # Get the values for human developed and agricultural areas
  # develop_vals <- gap@data@attributes[[1]]$VALUE_1[grepl("develop", gap@data@attributes[[1]]$NVC_CLASS, ignore.case = T)]
  # ag_vals <- gap@data@attributes[[1]]$VALUE_1[grepl("agricul", gap@data@attributes[[1]]$NVC_CLASS, ignore.case = T)]
  # water_vals <- gap@data@attributes[[1]]$VALUE_1[grepl("open water", gap@data@attributes[[1]]$NVC_CLASS, ignore.case = T)]
  # quarry_vals <- gap@data@attributes[[1]]$VALUE_1[grepl("quarries", gap@data@attributes[[1]]$NVC_CLASS, ignore.case = T)]
  # # quarries, mines, gravel pits, and oil wells
  # bad_vals <- c(as.character(develop_vals), as.character(ag_vals), as.character(water_vals), as.character(quarry_vals))
  # 
  # # Make GAP a raster of good and bad sampling areas
  # gap3 <- gap2
  # gap3[gap3 %in% bad_vals] <- 0
  # gap3[gap3 != 0] <- 1
  # 
  # # Combine gap3 and rsf, if the gap3 is 0, return 0, otherwise return rsf value
  # gap4 <- overlay(gap3, rsf2, fun = function(x, y){ifelse(x == 0, 0, y)})
  # 
  # # Aggregate
  # agg <- aggregate(gap4, 50, fun = mean)
  # 
  # # Select grid cells where the mean RSF value is at least 0.4
  # rsf2 <- agg
  # rsf2[rsf2 >= 0.4] <- 1
  # rsf2[rsf2 < 0.4] <- 0
  # 
  # # Clip this to Unit 4
  # winter <- mask(rsf2, gmu4)
  # 
  # # Clip out Indian Reservation
  # cdares <- readOGR("GIS Layers/Panhandle Features", "CDA_Indian_Reservation")
  # winter2 <- mask(winter, cdares, inverse = T)
  # 
  # # Take out any grid cells that are on Hwy 90
  # # Read in hwy layer
  # hwy <- readOGR("GIS Layers/Panhandle Features", "Hwy90")
  # # Find the raster cells the hwy passes through
  # tbd <- extract(winter2, hwy, cellnumbers = T)
  # # Change tbd from list to a matrix
  # tbd2 <- rbind(tbd[[1]], tbd[[2]])
  # # Set the cells on the hwy = 0
  # winter2[tbd2[, 1]] <- 0
  # # 
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
  
  # Find the cells to delete (Which() gives raster cell name)
  # del <- Which(winter, cells = T)[Which(winter, cells = T) %in% tbd2[, 1]]
