# Time to first event
# Anna Moeller 
# 3/23/2016

# # Load packages
# library(dplyr)
# library(lubridate)

# Time to event function
timetoevent_fn <- function(data, period, datelim, starttime = "12:00:00", species){
  # Takes: data from one camera, 
  # To do multiple cameras, call group_by(cam) first
  # Needs: data: a df with the following columns: cam, plot, timeLST, dateLST, __present = T/F
  #        period: sampling period in days
  #        starttime: what time of day to start the sampling period
  #        species: so far works for elk, md, wtd, human, prong
  # Makes a vector of sampling period start dates (every "period" days, from the 
  #   first full day to the last full day)
  # Groups by camera
  # Makes a df of time-to-first-event (or NA) (in hours) for each camera in each sampling period
  
  # Make sure dateLST and timeLST are the right class
  stopifnot(class(data$dateLST) == "Date" & class(data$timeLST) == c("POSIXct", "POSIXt"))
  
  # Make a vector of sampling period start dates (every "period" days, from the 
  #   first full day to the last full day)
  source("GitHub/CameraTrapStudy/Image Analysis/Time to event/Time to first event/sampling_start_fn.R")
  st <- sampling_start_fn(data = data, period = period, datelim = datelim, starttime = starttime)

  # Create a list of the time to first event for each camera in each sampling period
  source("GitHub/CameraTrapStudy/Image Analysis/Time to event/Time to first event/first_fn_2015.R")
  first <- first_fn(data = data, start = st, starttime = starttime, period = period, species = species)

  return(first)
}