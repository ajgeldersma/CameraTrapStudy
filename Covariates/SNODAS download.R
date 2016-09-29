  # Snow depth
  # National Operational Hydrologic Remote Sensing Center. 2004. Snow Data 
  #  Assimilation System (SNODAS) Data Products at NSIDC, [list the dates 
  #  of the data used]. Boulder, Colorado USA: National Snow and Ice Data Center. 
  #  http://dx.doi.org/10.7265/N5TB14TC
  # Use unmasked data

  # I think these are all the packages needed but I might be missing one
  library(stringr)
  library(rgdal)
  library(rwrfhydro) # For SNODAS functions
  library(gdalUtils) # For projecting and cropping raster

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