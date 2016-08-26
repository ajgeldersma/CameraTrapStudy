# combo_fn
# Anna Moeller
# 5/19/2015

# Combines Reconyx metadata with Timelapse Image Analyzer data (format 1)
combo_fn <- function(region, site, cam){

  # Set working directory
  setwd(paste("C:/Users/anna.moeller/Pictures/RECONYX Images", region, sep = "/"))
  
  # Read in data
  rcnx <- read.csv(paste(site, " Metadata/", cam, ".csv", sep = ""), as.is = T) # Reconyx metadata
  count <- read.csv(paste(cam, "ImageData.csv", sep = "/"), as.is = T) # Timelapse analyzer data
  
  # Stop if files are not same length
  if(length(rcnx[, 1]) != length(count[, 1])) stop("files are not the same length")
  
  # Join the two tables based on name of picture file
  tbl_df(rcnx)
  tbl_df(count)
  records <- inner_join(rcnx, count, by = c("Image.Name" = "Picture.Name"))
  
  # combo it up
  combo <- mutate(records, 
                  name = Image.Name,
                  site = Site,
                  cam = Location,
                  trigger = Trigger,
                  trig_type = substr(trigger, 1, 1),
                  dateLST = as.Date(Date.x, format = "%m/%d/%Y"),
                  timeLST = as.POSIXct(paste(Date.x, Time.x, sep = " "), format = "%m/%d/%Y %I:%M:%S %p"),
                  temp = Temp,
                  model = substr(Serial.., 1, 4))
  
  # Add GMT from the different time zones
  if(region == "Panhandle"){
    combo <- mutate(combo, 
                    timeGMT = timeLST + 28800) # From PST
  } else {
    combo <- mutate(combo, 
                    timeGMT = timeLST + 25200) # From MST
  }
  
  # Add "new event" and "end event" to event column
    # Make "Event" lowercase
    combo$event[combo$event == "Event"] <- "event"
    combo$event[combo$event == "New Event"] <- "new event"
  
    # Find all "events" after blanks (and ignore all "new events") and assign them "new event"
    combo$event[which(diff(combo$event == "event" | combo$event == "new event") == 1) + 1] <- "new event"
  
    # Find all non-blank and non-"new event" records before blanks or "new event" and assign them "end event"
    combo$event[which(diff(combo$event == "" | combo$event == "new event") == 1)] <- "end event"
  
  # Stop if there is not an "end event" for each "new event"
  stopifnot(sum(combo$event == "new event") == sum(combo$event == "end event"))
  
  # Create eventID
    # Index for empty pics
    missed <- combo$event == ""
  
    # Event id
    combo$eventID[combo$event == "new event"] <- 1
    combo$eventID[combo$event %in% c("event", "end event", "")] <- 0
    combo$eventID <- cumsum(as.numeric(combo$eventID))
    combo$eventID[missed] <- NA
  
  # Select columns for dataset
  combo <- select(combo, -Image.Name, -Site, -Location, -Trigger, -Date.x, -Time.x, -Temp, 
                  -Serial.., -Narrative, -Firmware, -Image.Path, -Location.Latitude, 
                  -Location.Longitude, -Date.y, -Time.y, -Folder.Name, -X)
  
  return(combo)
}
