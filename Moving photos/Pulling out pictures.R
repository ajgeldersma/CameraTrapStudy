  # Code for moving pictures that were taken at a given time into a new folder
  # Anna Moeller
  # 8/9/2017
  
  # Caveat: photos should have descriptive names before being moved. 
  #   If there are multiple IMG_0002.JPGs, they will not all make it to the new folder

  # Packages
  library(dplyr)
 
  # Example database
  # Required columns: SourceFile (a character vector), and datetime (a POSIXct vector)
  exm <- data.frame(SourceFile =
    c("F:/Camera Trap Photos/2015-16/Beaverhead/AM179/Trip 1/100RECNX/IMG_8749.JPG",
      "F:/Camera Trap Photos/2015-16/Beaverhead/AM203/Trip 1/100RECNX/IMG_2646.JPG",
      "F:/Camera Trap Photos/2015-16/Beaverhead/AM189/Trip 2/100RECNX/2016-03-13 12-55-00 T.JPG",
      "F:/Camera Trap Photos/2015-16/Beaverhead/AM177/Trip 2/100RECNX/IMG_0325.JPG",
      "F:/Camera Trap Photos/2015-16/Beaverhead/AM206/Trip 1/100RECNX/2016-02-21 08-55-00 T.JPG"),
                    datetime = as.POSIXct(c("2016-02-19 15:40:00", 
                                           "2016-01-24 14:32:00",
                                           "2016-03-13 12:55:10", 
                                           "2016-03-18 10:36:01", 
                                           "2016-02-21 08:55:00"),
                                          tz = "GMT", 
                                          format = "%Y-%m-%d %H:%M:%S"
                                 )
                    ) %>%
    mutate(SourceFile = as.character(SourceFile))

  # Find the photos in the interval you want
  sampint <- 5*60 # how often you want to sample, in seconds
  samplen <- 1*60 # how long you want to sample, in seconds
  toview <- exm %>%
    mutate(new = as.numeric(datetime) %% sampint) %>%
    filter(between(new, 0, samplen - 1))
    
  # Move those photos to a new home
  dest <- "C:/Users/Camera Trap Study/Photos to View"
  if(dir.exists(dest) == F) {
    dir.create(dest, recursive = T)
  }
  destfiles <- paste(dest, basename(toview$SourceFile), sep = "/")
  file.copy(from = toview$SourceFile, to = destfiles)
  
 