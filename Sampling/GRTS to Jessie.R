  # Creating winter range and sampling sites
  # Anna Moeller
  # 8/5/2015
  
  # Load packages
# I may not use all of these in this chunk of code I've included here...
  library(dplyr)
  library(rgdal) # readOGR, spTransform
  library(sp) # for SpatialPolygons, GridTopology, spTransform
  library(rgeos) # for gIntersection, gBuffer, etc. functions
  library(maptools) # for unionSpatialPolygons (dissolve function)
  library(raster) # for dealing with GAP raster
  library(spsurvey) # for GRTS
  
  ###########################################
  # Layers I bring in:
  # gmus: polygons shapefile of all the Game Management Units in Idaho
  # rsf: a raster shapefile of an RSF for elk in this area
  # gap: a raster shapefile of landcover types
  
  # Select only GMU 4 (my area of interest)
  gmus <- readOGR("GIS Layers/IDFG Game Management Units", "GameManagementUnits")
  gmu4 <- gmus[gmus$NAME == "4", ]
  
  # Read in RSF, define projection, crop to gmu4
  rsf <- raster("GIS Layers/Jon Horne/Elk Winter Range RSF.asc")
  proj4string(rsf) <- CRS("+proj=tmerc +lat_0=42 +lon_0=-114 +k=0.9996 +x_0=2500000 +y_0=1200000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0")
  rsf2 <- crop(rsf, extent(gmu4))
  
  # Aggregate (make 1x1m pixels into 50x50m squares)
  rsf3 <- aggregate(rsf2, 50, fun = mean)
  
  # Select grid cells where the mean RSF value is at least 0.4
  rsf3[rsf3 >= 0.4] <- 1
  rsf3[rsf3 < 0.4] <- 0
  
  # Bring in GAP raster, crop to extent of rsf2
  # Then I have to line up the rasters so the pixels start in the same place
  # This is very slow
  ### I am using resample here which isn't the best ####
  ### I tried bilinear but nearest neighbor was way better on keeping the 0s
  gap <- raster("GIS Layers/GAP.Idaho/gaplc_IDTM")
  gap2 <- resample(gap, rsf2, method = "bilinear")
  gap2 <- crop(gap2, extent(rsf2))
  tst1 <- resample(gap, rsf2, method = "ngb")
  saveRDS(tst1, file = "Camera Trap Study/GRTS/gap_output_panh.RData")
  gap2 <- readRDS("Camera Trap Study/GRTS/gap_output_panh.RData")
  
  # From GAP, find the values of landcovers I don't want
  # quarries, mines, gravel pits, and oil wells
  develop_vals <- gap@data@attributes[[1]]$VALUE_1[grepl("develop", gap@data@attributes[[1]]$NVC_CLASS, ignore.case = T)]
  ag_vals <- gap@data@attributes[[1]]$VALUE_1[grepl("agricul", gap@data@attributes[[1]]$NVC_CLASS, ignore.case = T)]
  water_vals <- gap@data@attributes[[1]]$VALUE_1[grepl("open water", gap@data@attributes[[1]]$NVC_CLASS, ignore.case = T)]
  quarry_vals <- gap@data@attributes[[1]]$VALUE_1[grepl("quarries", gap@data@attributes[[1]]$NVC_CLASS, ignore.case = T)]
  # Stick them all together
  bad_vals <- c(as.character(develop_vals), 
                as.character(ag_vals), 
                as.character(water_vals), 
                as.character(quarry_vals))
  
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
 