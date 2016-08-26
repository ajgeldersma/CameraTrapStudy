# Assign each elk event an event ID
# Anna Moeller
# 3/24/2016

# To do: 
# Make fn work for different species

eventID_fn <- function(data, species, cutoff = 30){
  # Takes timelapse + metadata database,
  # Adds an eventID column for every event that is greater than "cutoff" minutes apart
  # Accounts for change in camera
  # species: elk, md, wtd, prong, human

  if(species == "md" | species == "wtd"){
    stopifnot(!is.null(data$mdpresent) | !is.null(data$wtdpresent))
  }
  
  # Find the "present" column that corresponds with the target species
  prescol <- grep(paste0(species, "present"), names(data), ignore.case = T)
  
  # Take out the photos without the target species
  non <- filter(data, is.na(data[, prescol]) | data[, prescol] == F) %>%
    mutate(eventID = 0)
    
  # Start with "present" records only
  pres <- filter(data, !is.na(data[, prescol]) & data[, prescol] == T) %>%
        
    # Arrange the files in chronological order, by camera
    arrange(cam, timeLST) %>%
    
    # Calculate the time difference in minutes between "present"s
    mutate(elapse = difftime(timeLST, lag(timeLST), units = "mins"),
           
           # Change elapse from negative to positive when cam changes
           # Later we will use positive time changes to indicate a new event
           diffcam = c(0, ifelse(cam[2:length(cam)] == cam[1:(length(cam)-1)], 0, 1)),
           elapse = replace(elapse, diffcam == 1, 9999),

           # Create an eventID column
           eventID = 0,
           
           # Find where there is a gap of > "cutoff" min (including the first record)
           eventID = replace(eventID, c(1, which(elapse > cutoff)), 1),
           eventID = cumsum(eventID)) %>%
    
    # Join it back with the non-target species data
    select(-diffcam, -elapse) %>%
    bind_rows(., non) %>%
    arrange(cam, timeLST)
}

# # Test dataframe for debugging deer
# tst1 <- data.frame(mdbuck = c(0, 0, 0, 1, 0, 0, 1), 
#                   mdunkn = c(0, 0, 1, 0, 0, 0, 0), 
#                   wtdbuck = c(1, 0, 0, 0, 0, 0, 0), 
#                   wtdanterless = c(0, 0, 0, 0, 0, 0, 0),
#                   deerpresent = c(T, T, T, T, T, F, T), 
#                   cam = rep("AM1", 7), 
#                   timeLST = as.POSIXct(c("2016-01-01 12:00:00", 
#                                          "2016-01-01 12:05:00", 
#                                          "2016-01-01 12:10:00", 
#                                          "2016-01-01 12:11:00", 
#                                          "2016-01-01 12:12:00",
#                                          "2016-01-01 12:45:00",
#                                          "2016-01-01 14:30:00")))

# # Test dataframe for debugging elk
# tst <- data.frame(elkbull = c(1, 6, 0, 0, 0, 1, 0),
#                   elkcalf = c(0, 1, 0, 0, 0, 0, 1),
#                   elkpresent = c(T, T, T, T, F, T, T),
#                   cam = c(rep("AM1", 5), rep("AM2", 2)),
#                   timeLST = as.POSIXct(c("2016-01-01 12:00:00",
#                                          "2016-01-01 12:05:00",
#                                          "2016-01-01 12:10:00",
#                                          "2016-01-01 15:11:00",
#                                          "2016-01-01 15:12:00",
#                                          "2016-01-01 15:11:00",
#                                          "2016-01-02 14:30:00")))
