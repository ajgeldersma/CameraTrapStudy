# Workflow for image analysis
# Anna Moeller
# 5/19/2015

###### Need to manually delete first line of Reconyx output ######

# Load packages
library(dplyr)

# Initialize region and site
#region <- "Beaverhead Reg. 6" 
#region <- "Beaverhead Reg. 7" 
region <- "Panhandle"

#site <-"Deer Canyon" 
#site <- "Boulder Spring" 
#site <- "Buckhorn" 
#site <- "Kenney Creek" 
site <- "Swinnerton" 
#site <- "Italian Gulch"

# Set working directory
setwd(paste("C:/Users/anna.moeller/Pictures/RECONYX Images", region, site, sep = "/"))  

# Combine Reconyx metadata with Timelapse Image Analyzer data
### This is for the "no animals", "elk present"... format
source("C:/Users/anna.moeller/Dropbox/R Scripts/Image Analysis/combo_fn_TIA_format_2.R")
# Call combo_fn for all cameras: inputs: region, site, cam
cam <- grep("AM", list.files(), value = T) # cam = "AM41" for a single camera
pretty <- do.call(rbind, lapply(cam, combo_fn, region = region, site = site))

# TIA analysis check (still in progress)

# Create event ID (still in progress)
# Call event_ID_fn

# Check for collar (in progress)

# Time to first event (in progress)

# check_noon_fn checks for a noon photo every day and outputs dates where it failed
source("C:/Users/anna.moeller/Dropbox/R Scripts/Image Analysis/check_noon_fn.R")
# Call, inputs: data
noon_fail <- check_noon_fn(pretty)

# event_time_fn creates a new data.frame with event time (LST), duration
### This depends on me finishing a new way to do event ID
source("C:/Users/anna.moeller/Dropbox/R Scripts/Image Analysis/event_time_fn.R")
# Call, inputs: data
time <- event_time_fn(pretty)

# Motion vs. Time
### This depends on event ID too. 


################################################################################

#for(i in 1:length(unique(pretty$eventID[!is.na(pretty$eventID)]))){
#  start <- min(which(pretty$eventID == i))
#  end <- max(which(pretty$eventID == i))
#  time$length_min[i] <- difftime(pretty$timeGMT[end], pretty$timeGMT[start], units = "mins")
#}

#time <- data.frame(eventID = na.omit(unique(pretty$eventID)),
#                   start = as.POSIXct(tapply(pretty$timeGMT, pretty$eventID, min), origin = "1970-01-01 00:00:00"),
#                   end = as.POSIXct(tapply(pretty$timeGMT, pretty$eventID, max),origin = "1970-01-01 00:00:00"))
#duration <- time$end - time$start
#units(duration) <- "mins"
#time$duration <- round(as.numeric(duration), 2)

# Add event length to data.frame
#pretty$length_min <- NA
#for(i in 1:length(unique(pretty$eventID[!is.na(pretty$eventID)]))){
  #start <- min(which(pretty$eventID == i))
  #end <- max(which(pretty$eventID == i))
  #pretty$length_min[start:end] <- difftime(pretty$timeGMT[end], pretty$timeGMT[start], units = "mins")
#}