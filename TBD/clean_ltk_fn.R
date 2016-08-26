# Clean up Lotek collars
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
                serial = as.numeric(Device.ID),
                GMT = as.POSIXct(strptime(Date...Time..GMT., tz = "GMT", format = "%m-%d-%Y %H:%M:%S")),
                lat = as.numeric(Latitude),
                long = as.numeric(Longitude),
                alt = Altitude,
                fix = Fix.Status,
                tempC = Temp..C.) %>%
    select(serial, GMT, lat, long, alt, fix, DOP, tempC) 
  return(ltk)
}

# Other way to Kill dashed line under header
# ltk <- as.data.frame(sapply(raw.ltk, FUN = gsub, pattern = "^-+$", replacement = NA), 
#                      stringsAsFactors = F)
