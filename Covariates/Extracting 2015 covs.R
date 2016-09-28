  # Extracting 2015 covariate values
  # Anna Moeller
  # 6/24/2016
  
  # Covariates for N: elevation, slope, aspect, (USGS National Map)
  #  snow depth (SNODAS), forage quality (NDVI).
  # I also listed shrub cover (National Landcover Database). Was this from Jon? 
  
  # Covariates for p: IR flash type (lo-glow vs. no-glow),
  # ambient temperature, group size, time of day, and 
  # behavioral response (trap happy or trap shy) 
  
  # Load packages
  library(tidyr) # I don't really need this, but the order is important
  library(plyr) # I don't really need this, but the order is important
  library(raster) # This has to go before dplyr
  library(dplyr)
  library(rgdal)
  library(maptools) # for spRbind
  library(stringr)
  library(lubridate)
  
#################################################################
  # Plot-level covariates
  
  # Bring in sampling area shapefiles
  select30 <- readOGR("Camera Trap Study/Cameras/GPS GIS files/Sampling areas and plots/2015-16", 
                      "Beaverhead Selected Plots 2015-16")
  select6 <- readOGR("Camera Trap Study/Cameras/GPS GIS files/Sampling areas and plots/2015-16", 
                     "Panhandle Selected Plots 2015-16")
  # Unit 30 lost its projection somewhere along the way
  projection(select30) <- "+proj=tmerc +lat_0=42 +lon_0=-114 +k=0.9996 +x_0=2500000 +y_0=1200000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
  
  # Combine plots for units 30 and 6
    # First have to change the rownames so they'll fit together
    tst6 <- spChFIDs(select6, as.character(select6$plot))
    tst30 <- spChFIDs(select30, as.character(select30$plot))
    # Now you have to make sure all the columns are the same
    tst30$gaplc_IDTM <- NULL
    tst6$layer <- NULL
    # Now stick them together
    plots <- spRbind(tst6, tst30)
  
  # Bring in slope, aspect, elevation (30x30)
  elev <- raster("GIS Layers/1.GIS layers from Josh/dem")
  slope <- raster("GIS Layers/Slope/slope.tif")
  asp <- raster("GIS Layers/Aspect/aspect.tif")
  
  # Extract mean value to each plot
  plots$elev <- extract(elev, plots, fun = mean, na.rm = T)
  plots$slope <- extract(slope, plots, fun = mean, na.rm = T)
  plots$asp <- extract(asp, plots, fun = mean, na.rm = T)
  
  #### Bring in snow
  load("GitHub/Camera-trap-study/Covariates/SWErasterstack.RData")
  load("GitHub/Camera-trap-study/Covariates/SDrasterstack.RData")
  
  # How to plot one layer
  plot(swe, y = names(swe)[1])
  plot(swe@layers[[1]])
  
  # Extract snow at every plot on every day
  
  # plots_swe <- extract(swe, plots, fun = mean)
  # plots_snodep <- extract(sd, plots, fun = mean)
  # 
  # rownames(plots_snodep) <- plots$plot
  # sd2 <- as.data.frame(t(plots_snodep))
  # sd2$date <- as.Date(str_extract(rownames(sd2), "\\d{8}"), format = "%Y%m%d")
  # sd2$week <- week(sd2$date)
  
  # Save these somewhere!!!
  #save(swe2, file = "GitHub/Camera-trap-study/Covariates/swe_extract.RData")
  #save(sd2, file = "GitHub/Camera-trap-study/Covariates/snodep_extract.RData")
  load("GitHub/Camera-trap-study/Covariates/swe_extract.RData")
  load("GitHub/Camera-trap-study/Covariates/snodep_extract.RData")
  
  # To take a weekly average
  avg <- do.call(rbind, lapply(unique(swe2$week), function(x){
    apply(swe2[swe2$week == x, 1:18], 2, mean)
  }))
  rownames(avg) <- unique(swe2$week)
  
  ## NDVI
  # load("GitHub/Camera-trap-study/Covariates/NDVIrasterstack.RData")
  # plots_ndvi <- extract(NDVI, plots, fun = mean)
  # rownames(plots_ndvi) <- plots$plot
  # nd2 <- as.data.frame(t(plots_ndvi))
  # nd2$date <- as.Date(str_extract(rownames(nd2), "\\d{7}"), format = "%Y%j")
  
  #save(nd2, file = "GitHub/Camera-trap-study/Covariates/ndvi_extract.RData")
  load("GitHub/Camera-trap-study/Covariates/ndvi_extract.RData")
  
  ###########################################
  # Camera-level covariates
  
  # Bring in camera locations
  cams <- read.csv("Camera Trap Study/Cameras/GPS GIS files/Deployed Cameras 2015-16.csv")
  cams <- rename(cams, ele_gps = ele)
  cams <- SpatialPointsDataFrame(coords = cbind(cams$lon, cams$lat), data = cams, 
                                 proj4string = CRS("+init=epsg:4326"))
  cams <- spTransform(cams, CRS("+proj=tmerc +lat_0=42 +lon_0=-114 +k=0.9996 +x_0=2500000 +y_0=1200000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0"))
  
  # Bring in camera info
  load("GitHub/Camera-trap-study/2015 data/access.sum.RData")
  access <- access.sum
  
  # Extract spatial data
  # Compare GPS elevation and DEM
  cams$elev <- extract(elev, cams)
  
  # Stick the info together
  out <- as.data.frame(cams) %>%
    mutate(name = as.character(name)) %>%
    rename(lat_gps = lat,
           long_gps = lon) %>%
    left_join(access, ., by = c("camID" = "name")) %>%
    select(camID, plot, camnum, StudyArea, model, elev)
  
  
  ############################################
  # All the info for plot-level
  # Center and scale continuous variables
  xx <- as.data.frame(plots) %>%
    select(elev, slope, asp) %>%
    as.matrix(.) %>%
    scale(.)
  
  # Make a design matrix for categorical variables
  
  
  # Camera-level covariates
  yy <- as.data.frame(out) %>%
    mutate(elev = scale(elev))
  
  dm <- model.matrix(~model, yy)
  
  
  ################################################################
  # # Test extract
  # snowfiles <- list.files("GIS Layers/SNODAS/IDTM", pattern = "SWE", full.names = T)[1:3]
  # tst <- stack(snowfiles)
  # pts <- plots[plots$plot == "BH08",]
  # tmp <- extract(tst, pts, fun = mean)
