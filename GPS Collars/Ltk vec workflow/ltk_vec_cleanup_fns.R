# Combine and clean up lotek and vectronic collars
# Anna Moeller
# 5/19/2015

clean_ltk_fn <- function(date_yy){
  # Read in data
  file.ltk <- list.files(path = "Downloads/Lotek", full.names = T, 
                         pattern = paste("GPS_", date_yy, sep = ""))
  raw.ltk <- read.csv(file.ltk, header = T, as.is = T)
  
  # Kill the dashed line under the header
  ltk <- raw.ltk[-1, ]
  
  # Fix header, format columns, drop unwanted columns
  ltk <- mutate(ltk,
                serial = as.character(Device.ID),
                timeGMT = as.POSIXct(strptime(Date...Time..GMT., tz = "GMT", format = "%m-%d-%Y %H:%M:%S")),
                lat = as.numeric(Latitude),
                long = as.numeric(Longitude),
                alt = Altitude,
                fix = Fix.Status,
                tempC = Temp..C.) %>%
         select(serial, timeGMT, lat, long, alt, fix, DOP, tempC) 
  return(ltk)
}

clean_vec_fn <- function(date_yyyy){
  # Read in data (GPS only, not Mort if accidentally downloaded)
  path.vec <- paste("Downloads/Vectronic", date_yyyy, sep = "/")
  filenames.vec <- list.files(path = path.vec, full.names = T, pattern = "GPS_Collar")
  raw.vec <- bind_rows(lapply(filenames.vec, read.csv, as.is = T, row.names = NULL))
  
  # Pick columns I want and name them
  vec <- mutate(raw.vec,
                serial = as.character(CollarID), 
                timeGMT = as.POSIXct(strptime(paste(UTC_Date, UTC_Time), tz = "GMT", 
                                          format = "%m/%d/%Y %H:%M:%S")),
                lat = as.numeric(Latitude....), 
                long = as.numeric(Longitude....), 
                alt = Height..m., 
                fix = FixType, 
                tempC = Temp...C.) %>%
    select(serial, timeGMT, lat, long, alt, fix, DOP, tempC)
  return(vec)
}

ltk_vec_combo_fn <- function(extent = "Idaho", DOPfilt = 6.0, start = "2014-12-01", deploy15 = T){
# Combine Lotek and Vectronic data frames
  elk <- rbind(ltk, vec) %>%
    # Add Local Time
    ###### This 25200 takes 7 hours off GMT. Doesn't account for PST/MST/daylight savings
    mutate(timeLST = timeGMT - 25200,
          dateGMT = as.Date(timeGMT),
          dateLST = as.Date(timeLST)) %>%
    
    #### create Lookup table GMU, tz
    # gmu$ID[over(elk_sp, gmu)]
    # LU$tz[match(elk_sp$gmu, LU$GMU)]
    # beginCluster(n = 6, ty[e = "sock"])
    # raster::extract(elk_sp, gmu)
    # endCluster()
    
    # Filter based on inputs
    arrange(serial) %>%
    filter(DOP <= DOPfilt,
          dateGMT > as.Date(start))
  
  # Filter by lat/long
  if(extent == "Idaho"){
    elk <- filter(elk,
                  !is.na(lat) & lat > 42 & lat < 49,
                  !is.na(long) & long > -118 & long < -111)
  }
#   
#   # Select only elk collars on my list of collars deployed in 2015
#   #### I wonder if this is right. Check this ######
#   if(deploy15){ 
#     serial <- read.csv("List of GPS Elk Collars 01282015.csv", as.is = T)
#     elk <- elk[elk$serial %in% serial$Serial, ]
#   }
  return(elk)
}

delete_cap <- function(data){
  # Delete all fixes before the animal was captured and after it died
  
  #print(data$serial[1])
  
  # Load elk captures and mortalities/censors
  cap <- read.csv("C:/Users/anna.moeller/Documents/Camera Trap Study/GPS Collars/Elk Idaho Cap Fate 2013-2015.txt",
                  as.is = T)
  cap <- mutate(cap, 
                serial = as.character(Collar_Serial_No),
                capdate = as.Date(Capture_Date, format = "%m/%d/%Y"),
                mortdate = as.Date(FateDate, format = "%m/%d/%Y"),
                censordate = as.Date(CensorDate, format = "%m/%d/%Y"),
                enddate = NA,
                enddate = replace(enddate, !is.na(mortdate), mortdate[!is.na(mortdate)]), 
                enddate = replace(enddate, !is.na(censordate), censordate[!is.na(censordate)]),
                enddate = as.Date(enddate, "1970-01-01")) %>%
    select(serial, capdate, mortdate, censordate, enddate)

#   # Which collars are not in the capture db?
#   serial <- unique(data$serial)
#   serial[which(!(serial %in% cap$serial))]
  
  # Keep records starting the day after capture until the enddate
  tmp <- left_join(data, cap, by = c("serial" = "serial")) %>%
    mutate(enddate = replace(enddate, is.na(enddate), max(dateLST, na.rm = T))) %>%
    filter(dateLST > capdate & dateLST <= enddate)  

  return(tmp)
}

