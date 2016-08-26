# combo_fn
# Anna Moeller
# 5/19/2015

# Combines Reconyx metadata with Timelapse Image Analyzer data (version 1)
combo_timelapse1_fn <- function(region, site, cam){
  
  # Read in data
  rcnx <- read.csv(paste(region, "/", site, " Metadata/", cam, ".csv", sep = ""), as.is = T) # Reconyx metadata
  count <- read.csv(paste(region, cam, "ImageData.csv", sep = "/"), as.is = T) # Timelapse analyzer data
  
  # Stop if files are not same length
  if(length(rcnx[, 1]) != length(count[, 1])) stop("files are not the same length")
  
  # Give both files a unique cam + pic file name
  rcnx$file <- paste(rcnx$Location, rcnx$Image.Name)
  count$Folder.Name <- gsub("[A-Z]{2} (AM[0-9]*)", "\\1", count$Folder.Name) # I had to do this because some are in format "BS AM58" and some are in "AM58"
  count$file <- paste(count$Folder.Name, count$Picture.Name, sep = " ")
  
  # Stop if these aren't the same thing
  stopifnot(!(any(rcnx$file != count$file)))
  
  # Join the two tables based on name of picture file
  tbl_df(rcnx)
  tbl_df(count)
  records <- inner_join(rcnx, count, by = c("file" = "file"))
  
  # Stop if tables didn't join
  if(dim(records)[1] == 0) stop("tables didn't join")
  
  # combo it up
  combo <- mutate(records, 
                  region = Site,
                  site = site,
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
                  -Location.Longitude, -Picture.Name, -Date.y, -Time.y, -Folder.Name, -X)
  
  return(combo)
}

################################################################################

# Combines Reconyx metadata with Timelapse Image Analyzer data (version 2.0)
combo_timelapse2_fn <- function(region, site, cam){
  
  # Read in data
  rcnx <- read.csv(paste(region, "/",  site, " Metadata/", cam, ".csv", sep = ""), as.is = T) # Reconyx metadata
  count <- read.csv(paste(region, cam, "TimelapseData.csv", sep = "/"), as.is = T) # Timelapse analyzer data
  
  # Stop if files are not same length
  if(length(rcnx[, 1]) != length(count[, 1])) stop("files are not the same length")
  
  # Give both files a unique cam + pic file name
  rcnx$file <- paste(rcnx$Location, rcnx$Image.Name)
  count$Folder <- gsub("[A-Z]{2} (AM[0-9]*)", "\\1", count$Folder) # I had to do this because some are in format "BS AM58" and some are in "AM58"
  count$file <- paste(count$Folder, count$File)
  
  # Stop if these aren't the same thing
  stopifnot(!(any(rcnx$file != count$file)))
  
  # Join the two tables based on name of picture file
  tbl_df(rcnx)
  tbl_df(count)
  records <- inner_join(rcnx, count, by = c("file" = "file"))
  
  # Stop if tables didn't join
  if(dim(records)[1] == 0) stop("tables didn't join")
  
  # make it pretty
  combo <- mutate(records, 
                  region = Site,
                  site = site,
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
                  -Location.Longitude, -File, -Folder, -Date.y, -Time.y, -X)
  
  return(combo)
}