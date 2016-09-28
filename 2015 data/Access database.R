  # Access db 2015
  # Anna Moeller
  # 4/8/2016
  
  # Load packages
  library(dplyr)
  
  # Bring in the Access database
  acc <- read.table("C:/Users/anna.moeller/Documents/Camera Trap Study/Cameras/Important Info 2015-16.txt", header = T, sep = ",",  as.is = T)
  
  # Create a lookup table for plotnum
  plotnum <- data.frame(plot = c(unique(acc$PlotNum[acc$StudyArea == "Beaverhead"]),
                                 unique(acc$PlotNum[acc$StudyArea == "Saint Joe"])),
                        plotnum = rep(1:9, 2)) %>%
    mutate(plot = as.character(plot))
  
  # Clean up access database to look how I want
  access <- rename(acc,
                   deploy = Deployment.Date,
                   takedown = TakeDownDate,
                   camID = Camera.ID,
                   site = StudyArea,
                   plot = PlotNum,
                   battcheck = Battery.Check.Date,
                   op.start = Operating.Start,
                   op.end = Operating.End,
                   plot.start = PlotStartDate,
                   plot.end = PlotEndDate) %>%
    mutate(site = replace(site, site == "Saint Joe", "St. Joe"),
           deploy = as.Date(deploy, format = "%m/%d/%Y %H:%M:%S"),
           takedown = as.Date(takedown, format = "%m/%d/%Y %H:%M:%S"),
           battcheck = as.Date(battcheck, format = "%m/%d/%Y %H:%M:%S"),
           op.start = as.Date(op.start, format = "%m/%d/%Y %H:%M:%S"),
           op.end = as.Date(op.end, format = "%m/%d/%Y %H:%M:%S"),
           plot.start = as.Date(plot.start, format = "%m/%d/%Y %H:%M:%S"),
           plot.end = as.Date(plot.end, format = "%m/%d/%Y %H:%M:%S")) %>%
    left_join(., plotnum, by = c("plot" = "plot"))
  # Added two nonexistent cameras to BH03, SJ09 (AM999, AM888) in Access db
  
  # That is a long db. If we don't need that info, use summary:
  access.sum <- group_by(access, camID) %>%
    summarise(camnum = min(Camnum),
              site = first(site),
              easting = min(Easting),
              northing = min(Northing),
              op.start = min(op.start),
              op.end = max(op.end),
              plot.start = min(plot.start),
              plot.end = max(plot.end),
              plot = min(plot),
              plotnum = min(plotnum),
              model = min(Camera.Model)) %>%
    mutate(model = replace(model, model == "", NA))
  
  save(access, file = "C:/Users/anna.moeller/Documents/GitHub/CameraTrapStudy/2015 data/access.RData")
  save(access.sum, file = "C:/Users/anna.moeller/Documents/GitHub/CameraTrapStudy/2015 data/access.sum.RData")
  
  ###################################################################
  
  
  # # Fill in blanks with NAs (this turns it into a matrix, so turn it back)
  # access2 <- apply(access, 2, function(x){
  #   gsub("^$", NA, x)
  # })
  # access2 <- as.data.frame(access2)
  # 
  # # There is an empty row so take that out. Make everything the right class again.
  # access2 <- filter(access2, !is.na(Camera.ID)) %>%
  #   mutate(Easting = as.numeric(Easting),
  #          Northing = as.numeric(Northing))
  ### This screws absolutely everything up. 
  
  # Plot Easting and Northing to make sure they're right
  beav <- access[access$StudyArea == "Beaverhead",]
  panh <- access[access$StudyArea == "Saint Joe",]
  
  # Plot
  plot(panh$Easting, panh$Northing)
  plot(beav$Easting, beav$Northing)
  
  # Compare these graphs to the GPS points, to see if things were entered correctly
  gps <- read.csv("C:/Users/anna.moeller/Documents/Camera Trap Study/Cameras/GPS GIS files/Deployed Cameras 2015-16.csv", as.is = T)
  
  # Add site in order to plot
  gps$site[gps$lat < 45.5] <- "Beaverhead"
  gps$site[gps$lat > 45.5 & gps$lat < 47.5] <- "St. Joe"
  gps$site[gps$lat > 47.5] <- "Kellogg"
  
  # Plot
  plot(gps$lon[gps$site == "St. Joe"], gps$lat[gps$site == "St. Joe"])
  plot(gps$lon[gps$site == "Kellogg"], gps$lat[gps$site == "Kellogg"])
  plot(gps$lon[gps$site == "Beaverhead"], gps$lat[gps$site == "Beaverhead"])
  
