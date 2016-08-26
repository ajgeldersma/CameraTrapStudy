# combo_fn
# Anna Moeller
# 5/19/2015

# Combines Reconyx metadata with Timelapse Image Analyzer data (format 2)
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
                  timeLST = as.POSIXct(paste(Date.x, Time.x, sep = " "), format = "%m/%d/%Y %I:%M:%S %p", tz = "MST"),
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
  
  # Select columns for dataset
  combo <- select(combo, -Image.Name, -Site, -Location, -Trigger, -Date.x, -Time.x, -Temp, 
                  -Serial.., -Narrative, -Firmware, -Image.Path, -Location.Latitude, 
                  -Location.Longitude, -Date.y, -Time.y, -Folder.Name, -X)
  
  return(combo)
}