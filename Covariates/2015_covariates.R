  # 2015 Covariates
  # Anna Moeller
  # 5/26/2016
  
  # Covariates for N: elevation, slope, aspect, (USGS National Map)
  #  snow depth (SNODAS), forage quality (NDVI).
  # I also listed shrub cover (National Landcover Database). Was this from Jon? 
  
  # Covariates for p: IR flash type (lo-glow vs. no-glow),
  # ambient temperature, group size, time of day, and 
  # behavioral response (trap happy or trap shy) 
  
  # Load packages
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(rwrfhydro) # For SNODAS functions
  library(MODISTools) # For downloading NDVI
  library(rgdal) # readOGR, spTransform
  library(sp) # for SpatialPolygons, GridTopology, spTransform
  library(rgeos) # for gIntersection, gBuffer, etc. functions
  library(maptools) # for unionSpatialPolygons (dissolve function)
  library(raster) 
  library(rts) # For raster time series
  library(gdalUtils) # For projecting and cropping raster
  
  # Set working directory
  setwd("C:/Users/anna.moeller/Documents")
  
###############################################
  # Bring in camera plots and points
  # Load GMU and Zone polygon shapefiles
  gmus <- readOGR("GIS Layers/IDFG Game Management Units", "GameManagementUnits")
  zones <- readOGR("GIS Layers/IDFG Elk Management Zones", "ElkManagementZones")
  
  # Make a shape for GMU 30 and 6
  gmu30 <- gmus[gmus$NAME == "30", ]
  gmu6 <- gmus[gmus$NAME == "6", ]
  gmus <- gmus[gmus$NAME == "6"|gmus$NAME == "30", ]
  zones <- zones[zones$NAME == "Panhandle"|zones$NAME == "Beaverhead", ]
  
  # Bring in sampling area shapefiles
  samp30 <- readOGR("Camera Trap Study/Cameras/GPS GIS files/Sampling areas and plots", 
                    "Beaverhead Sampling Area 2015-16")
  select30 <- readOGR("Camera Trap Study/Cameras/GPS GIS files/Sampling areas and plots", 
                      "Beaverhead Selected Plots 2015-16")
  thirds30 <- readOGR("Camera Trap Study/Cameras/GPS GIS files/Sampling areas and plots", 
                      "Beaverhead Selected Plots Thirds 2015-16")
  samp6 <- readOGR("Camera Trap Study/Cameras/GPS GIS files/Sampling areas and plots", 
                   "Panhandle Sampling Area 2015-16")
  select6 <- readOGR("Camera Trap Study/Cameras/GPS GIS files/Sampling areas and plots", 
                     "Panhandle Selected Plots 2015-16")
  thirds6 <- readOGR("Camera Trap Study/Cameras/GPS GIS files/Sampling areas and plots", 
                     "Panhandle Selected Plots Thirds 2015-16")
  
  # Thirds30 lost part of its projection along the way.
  projection(thirds30) <- "+proj=tmerc +lat_0=42 +lon_0=-114 +k=0.9996 +x_0=2500000 +y_0=1200000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
  projection(samp30) <- "+proj=tmerc +lat_0=42 +lon_0=-114 +k=0.9996 +x_0=2500000 +y_0=1200000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
  projection(select30) <- "+proj=tmerc +lat_0=42 +lon_0=-114 +k=0.9996 +x_0=2500000 +y_0=1200000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
  
  # writeOGR(sampthirds, "Camera Trap Study/Cameras/GPS GIS files/Sampling areas and plots", 
  #          "Beaverhead Selected Plots Thirds 2015-16", driver = "ESRI Shapefile")
  
  # Bring in camera locations
  cams <- read.csv("Camera Trap Study/Cameras/GPS GIS files/Deployed Cameras 2015-16.csv")
  cams <- SpatialPointsDataFrame(coords = cbind(cams$lon, cams$lat), data = cams, 
                                 proj4string = CRS("+init=epsg:4326"))
  cams <- spTransform(cams, CRS("+proj=tmerc +lat_0=42 +lon_0=-114 +k=0.9996 +x_0=2500000 +y_0=1200000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0"))
  
  # Checking things out
  plot(gmu30)
  plot(samp30, add = T)
  plot(select30, add = T, col = "red")
  plot(cams, add = T, pch = 16)
  
  plot(gmu6)
  plot(samp6, add = T)
  plot(select6, add = T, col = "red")
  plot(cams, add = T, pch = 16)
  
  plot(select30[select30$plot == "BH01",])
  plot(thirds30, add = T)
  plot(cams, add = T, pch = 16)
  
  plot(select6[select6$plot == "SJ09",])
  plot(thirds6, add = T)
  plot(cams, add = T, pch = 16)
  
#######################################################
  # DEM
  
  # Elevation
  # Bring in DEM (30x30) raster from Josh
  dem <- raster("GIS Layers/1.GIS layers from Josh/dem")
  
  # Slope
  slope <- terrain(dem, opt = "slope") # terrain() is a function in raster
  
  # Aspect
  asp <- terrain(dem, opt = "aspect")
  
  # Write these as rasters, so they refer to a permanent file
  writeRaster(slope, path.expand("~/GIS Layers/Slope/slope"), format = "GTiff")
  writeRaster(asp, path.expand("~/GIS Layers/Aspect/aspect"), format = "GTiff")
  
  # Save elevation as .RData
  save(dem, file = "C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Covariates/dem.RData")
  
##########################################################################
  # Snow depth
  # National Operational Hydrologic Remote Sensing Center. 2004. Snow Data 
  #  Assimilation System (SNODAS) Data Products at NSIDC, [list the dates 
  #  of the data used]. Boulder, Colorado USA: National Snow and Ice Data Center. 
  #  http://dx.doi.org/10.7265/N5TB14TC
  # Use unmasked data
  
  # Download all the rasters and make them Netcd files
  #   Why? I don't know. But that's what these functions create
  snodasPath <- path.expand("~/GIS Layers/SNODAS/Raw/")
  dates <- seq.Date(from = as.Date("2015-12-15"), to = as.Date("2016-4-27"), by = "day")
  
  snow <- function(date, path){
    # Download SNODAS for a given day and create a Netcdf file from it
    GetSnodasDepthSweDate(date, outputDir = path)
    snodasList <- ReadSnodasDepthSweDate(date, outputDir = path)
    PutSnodasNcdf(snodasList, outputDir = path)
  }
  lapply(dates, snow, path = snodasPath)
  
  # Make projected rasters for Idaho
  setwd("C:/Users/anna.moeller/Documents/GIS Layers/SNODAS")
  zones <- readOGR("GIS Layers/IDFG Elk Management Zones", "ElkManagementZones")
  snowIDTM.fn <- function(path_in, varname, crop_shp) {
    date <- str_extract(path_in, pattern = "\\d{8}")
    tmp <- raster(path_in, varname = varname)
  
    # Give it a projection and extent (from coordinates online)
    projection(tmp) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    tmp@extent <- extent(-124.7337, -66.9421, 24.9504, 52.8754)
    
    # Write this as a tif file so we can use gdal
    writeRaster(tmp, "temp/out.tif", format = "GTiff")
    
    # Project it to IDTM using gdal
    tmp_in <- file.path(getwd(), "temp/out.tif")
    out <- file.path(getwd(), "IDTM", paste0(varname, "_", date, ".tif"))
    gdalwarp(srcfile = tmp_in, 
             dstfile = out,
             s_srs = projection(tmp),
             t_srs = projection(crop_shp),
             overwrite = T)
    new <- raster(out)
    new2 <- crop(new, crop_shp)
    #new3 <- mask(new2, crop_shp)
    writeRaster(new3, out, format = "GTiff", overwrite = T)
    file.remove("temp/out.tif")
  }
  
  snow_raw <- list.files("Raw", pattern = "SNODAS", full.names = T)
  lapply(snow_raw, snowIDTM.fn, varname = "SWE", crop_shp = zones)
  lapply(snow_raw, snowIDTM.fn, varname = "snowDepth", crop_shp = zones)
  
  # Read SWE and SD in as a raster stack
  swe <- stack(list.files("IDTM", pattern = "SWE", full.names = T))
  sd <- stack(list.files("IDTM", pattern = "snowDepth", full.names = T))
  
  # How to plot one layer
  plot(swe, y = names(swe)[1])
  plot(swe@layers[[1]])
  
  save(swe, file = "C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Covariates/SWErasterstack.RData")
  save(sd, file = "C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Covariates/SDrasterstack.RData")
  
#####################################################################
  # NDVI 
  # Script from Josh
  setwd("C:/Users/anna.moeller/Documents/GIS Layers/MODIS")
  
  # Find a reference file
  zones <- readOGR("GIS Layers/IDFG Elk Management Zones", "ElkManagementZones")
  
  #  Product Short Name, get from MODIS products table
  product <- "MOD13Q1"
  
  #  Tiles, I looked this up on the web
  h = c(9, 10)
  v = 4
  
  #  The MODIS tools package is great for getting MODIS data at a point, 
  #  but in this case I wanted all the data for an entire state.  I called
  #  the useful GetDates function by namespace to get the available dates 
  #  for the product of interest.  The Lat and Long were taken from 
  #  Google Earth and represent the center of the state of interest.
  dts <- MODISTools::GetDates(Product = product, 
                              Lat = 45, 
                              Long = -115)
  
  #  The dates now look like A2000001, which is the 4 digit year + julian
  #  day, but I need a POSIX like date
  dts <- as.Date(gsub("A", "", dts), "%Y%j")
  #  Dates need to be in a different format
  #dts_in <- paste(unique(format(dts, "%Y")), "01", "01", sep = ".")
  dts_in <- format(dts, "%Y.%m.%d")
  #  Subset to dates I want
  dts_in <- dts_in[362:371]
  
  #  Download the hdf5 files using the getMODIS function from the rts 
  #  package.
  getMODIS(product, h = h, v = v, dates = dts_in, version = "006")
  
  #  Get hdf names, the downloaded files were saved in our working 
  #  directory that we set at the beginning of the script
  hdfs <- list.files(pattern = "hdf$")
  
  #  Create new file names, in this case we will make geoTIFFs
  gtiffs <- gsub("hdf", "tif", hdfs)
  
  #  Define projections
  from_proj <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs" 
  to_proj <- projection(zones)
  
  #  Convert hdf to tif using gdal
  for(i in 1:length(hdfs)){
    gdal_translate(hdfs[i], gtiffs[i], sd_index = 2)
  }
  
  #  Project and Mosaic tiffs - gdalwarp
  gtif2mos <- unique(substr(gtiffs, 10, 16))
  for(i in 1:length(gtif2mos)){
    tmp <- gtiffs[grepl(gtif2mos[i], gtiffs)]
    gdalwarp(tmp, 
             dstfile = file.path(getwd(), "Mosaicked", 	
                                 paste0("MOD13Q1_", gtif2mos[i], ".tif")),
             s_srs = from_proj,
             t_srs = to_proj,
             srcnodata = paste(as.character(65529:65535), collapse = ","),
             dstnodata = "9999",
             overwrite = T)
  }
  
  fnms <- file.path(getwd(), "Mosaicked", list.files(file.path(getwd(), 
                                                               "Mosaicked")))
  
  #  Clip raster and then scale values
  cropped <- lapply(1:length(fnms), function(i){
    r <- raster(fnms[i])
    r2 <- crop(r, zones)
    r2 <- r2 * 0.0001
    writeRaster(r2, file.path(getwd(), "Mosaicked", 	
                              paste0("MOD13Q1_", gtif2mos[i], ".tif")),
                format = "GTiff", overwrite = T)
  })
  
  #  Create a raster stack
  NDVI <- stack(fnms)
  
  #  Calculate summary statistics, if desired
  npp_mu <- calc(NDVI, fun = mean, na.rm = T)
  
  #  Save NDVI raster stack
  save(NDVI, file = "C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Covariates/NDVIrasterstack.RData")
  
################################################################################
  # Shrub Cover
  
##########################################################
  # Land cover??? I didn't list this but here it is anyway
  # Bring in GAP raster, crop to extent of GMU 30
  gap <- raster("GIS Layers/GAP.Idaho/gaplc_IDTM")
  gap2 <- crop(gap, extent(gmus))
  
  # Move from attribute table to simple values, use VALUE_1
  gap3 <- deratify(gap2, att = "VALUE_1", complete = T)