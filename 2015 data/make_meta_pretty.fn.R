# Make_meta_pretty.fn
# Anna Moeller
# 5/10/16

# Pull out the info I want from the metadata files

make_meta_pretty.fn <- function(meta){
  
  # Some of the PC800s don't have Make, Model, or Flash columns
  # And the fucking ultrafires don't have serial numbers. 
  
  # Add Make, Model, and Flash to the PC800s that don't have these
  #   Have to find Beaverhead cameras first because UXR don't have SerialNumber column
  if(grepl("Beaverhead", meta$SourceFile[1])){
    if(grepl("^P800", meta$SerialNumber[1])){
      meta$Make <- "RECONYX"
      meta$Model <- "PC800 PROFESSIONAL"
      meta$Flash <- NA      
    }
  }
  
  if(meta$Model[1] == "UltraFire"){
    # For ultrafires from exifr
    meta <- select(meta, SourceFile, FileName, Make, Model, DateTimeOriginal, Flash,
                   Software, Megapixels, ShutterSpeedValue) %>%
      rename(make = Make,
             model = Model,
             FirmwareVersion = Software) %>%
      mutate(site = gsub(".*2015-16/(.*)/AM.*", "\\1", SourceFile),
             cam = gsub(".*(AM.*)/Trip.*", "\\1", SourceFile),
             timeLST = as.POSIXct(DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S", tz = "GMT"),
             dateLST = as.Date(timeLST),
             timeGMT = timeLST + 8*60*60)
    # I am 90% sure this time zone setup works. It reads "PDT" but it's not actually changing
    # No moon phase, no temp, no battery voltage, no trigger type. 
    #   Make sure trigger type says T not TRUE
    
  } else {
    
    # For Hyperfires: 
    # Make sure Trigger says T not TRUE
    # Make sure cam does not have spaces after it
    meta1 <- select(meta, SourceFile, FileName, Make, Model, DateTimeOriginal, Flash,
                   FirmwareVersion, TriggerMode, Sequence, MoonPhase,
                   AmbientTemperature, InfraredIlluminator, MotionSensitivity, 
                   BatteryVoltage, UserLabel, Megapixels, ShutterSpeed) %>%
      rename(make = Make,
             model = Model,
             sequence = Sequence,
             trigger = TriggerMode,
             tempC = AmbientTemperature,
             cam = UserLabel) %>%
      mutate(FirmwareVersion = gsub(" ", ".", FirmwareVersion),
             timeLST = as.POSIXct(DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S", tz = "GMT"),
             dateLST = as.Date(timeLST),
             timeGMT = timeLST + 7*60*60,
             sequence = gsub(" ", "/", sequence),
             site = gsub(".*2015-16/(.*)/AM.*", "\\1", SourceFile),
             trigger = substr(as.character(trigger), 1, 1),
             cam = str_extract(cam, "AM\\d{2}\\d?"))
  }
}


# All of these things are there, in Mapview. Try a different tool to extract metadata
# Tried exifr, tried exiftool stand-alone

# # For HC600:
#hc <- read.csv("F:/Camera Trap Photos/2015-16/Beaverhead/AM23/Trip 1/100RECNX/Metadata.csv")
# # For PC800: 
#pc1 <- read.csv("F:/Camera Trap Photos/2015-16/Beaverhead/AM52/Trip 1/100RECNX/Metadata.csv")
# # For PC900: 
# pc2 <- read.csv("G:/Camera Trap Photos/2015-16/Beaverhead/AM68/Trip 1/100RECNX/Metadata.csv")
# # For Ultrafire:
# ult <- read.csv("G:/Camera Trap Photos/2015-16/St. Joe/AM92/Trip 1/100RECNX/Metadata.csv")
# # From Mapview

########## Things I don't need anymore
## Both of these worked, but I ended up just using GMT so no dates/times were changed. 
#timeLST = as.POSIXct(timeLST, format = "%Y:%m:%d %H:%M:%S", tz = "MST")

#timeLST = paste(as.character(timeLST), "US/Pacific"),
#timeLST = as.POSIXct(timeLST, format = "%Y:%m:%d %H:%M:%S", tz = "US/Pacific")