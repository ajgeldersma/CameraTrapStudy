# Assign each elk event an event ID
# Anna Moeller
# 3/24/2016

event_ID_fn <- function(data, cutoff = 30){
  # Takes timelapse/metadata file,
  # Adds an eventID column for every event that is greater than "cutoff" minutes apart
  # This should work okay because diff between consecutive cameras is negative
  
  # Let's make sure the files are arranged in chronological order, by camera
  data <- arrange(data, file)
  
  # Start with "elk present" records only
  pres <- data[data$present == "elk present", ]
  
  # Calculate the time difference in minutes between "elk present"s
  pres$difftime <- c(0, difftime(time1 = pres$timeLST[2:length(pres$timeLST)],
                                 time2 = pres$timeLST[1:(length(pres$timeLST)-1)],
                                 units = "mins"))
  
  # Make sure the time change is positive when the camera changes
  pres$diffcam <- c(0, ifelse(pres$cam[2:length(pres$cam)] == pres$cam[1:(length(pres$cam)-1)], 0, 1))
  pres$difftime[pres$diffcam == 1] <- 999

  # Find where there is a gap of > "cutoff" min (including the first record)
  eventstart <- c(1, which(pres$difftime > cutoff))
  
  # Make the column eventID
  pres$eventID <- 0
  
  # Give each event an ID
  pres$eventID[eventstart] <- 1
  pres$eventID <- cumsum(as.numeric(pres$eventID))
  
  # Join it back up with the non-elk photos
  non <- data[data$present != "elk present", ]
  non$eventID <- 0
  non$difftime <- 0
  out <- bind_rows(pres, non)
  out <- arrange(out, file)
  out <- select(out, -difftime, -diffcam)
}