  # IDFG subunits
  # Anna Moeller
  # 7/20/2016
  
  # Load packages
  library(raster)
  library(rgdal)
  library(rgeos)
  
  # Load up the layers
  sub <- readOGR("GIS Layers/IDFG GMU Subunits", "idfgsubunits")
  
  # Look at the subunits just in GMU 30
  sub30 <- sub[sub$UNIT == "30", ]
  
  # Calculate the area of each subunit and put in square miles
  areas <- sapply(slot(sub30, "polygons"), slot, "area")
  ### This is slightly different from the area calculated in the dataframe
  #   (biggest difference = 0.52 mi2)
  sqmi <- areas * 3.861e-7
  # Subunits in GMU 30 are 1.1 - 14.3 mi^2
  
  # Bring in my plots
  plots <- readOGR("Camera Trap Study/Cameras/GPS GIS files/Sampling areas and plots", 
                   "Beaverhead Selected Plots 2015-16")
  oldplot <- readOGR("Camera Trap Study/Cameras/GPS GIS files/2014-15", "Beaverhead 2014-15")
  
  # Create subunit labels
  xy_labs <- gCentroid(sub30, byid = T)
  labs <- as.character(sub30$SUBUNIT)
  
  # Plot
  par(mar = c(0,0,0,0))
  plot(sub30)
  plot(plots, add = T, border = "red", lwd = 2)
  plot(oldplot, add = T, col = "dodgerblue")
  text(xy_labs$x, xy_labs$y, labs, cex = .7)
  
  # Other random stuff Josh was playing around with 
  library(zoom)
  sq.zoom(sub30)
  library(dplyr)
  library(leaflet)
  s30 <- spTransform(sub30, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  leaflet(s30) %>% addTiles() %>% addPolygons(stroke = T, color = c("crimson"))
  
  # Bring in mule deer points for Charlie
  deer <- read.csv("C:/Users/anna.moeller/Desktop/winter_comp_2016.csv")
  s30 <- spTransform(sub30, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  p30 <- spTransform(plots, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  
  
