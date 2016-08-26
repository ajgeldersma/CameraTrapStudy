# Clean up Vectronic collars
# Anna Moeller 
# 5/19/2015

clean_vec_fn <- function(date_yyyy){
  # Read in data (GPS only, not Mort if accidentally downloaded)
  path.vec <- paste("Downloads/Vectronic", date_yyyy, sep = "/")
  filenames.vec <- list.files(path = path.vec, full.names = T, pattern = "GPS_Collar")
  raw.vec <- do.call("rbind", lapply(filenames.vec, read.csv, as.is = T, row.names = NULL))

  # Pick columns I want and name them
  vec <- mutate(raw.vec,
                serial = as.numeric(CollarID), 
                GMT = as.POSIXct(strptime(paste(UTC_Date, UTC_Time), tz = "GMT", 
                                          format = "%m/%d/%Y %H:%M:%S")),
                lat = as.numeric(Latitude....), 
                long = as.numeric(Longitude....), 
                alt = Height..m., 
                fix = FixType, 
                tempC = Temp...C.) %>%
  select(serial, GMT, lat, long, alt, fix, DOP, tempC)
  return(vec)
}