# Time to first event
# Anna Moeller 
# 7/1/2015

# Load packages
library(dplyr)
library(lubridate)

# Time to event function
timetoevent.fn <- function(data, period, name, starttime = "12:00:00"){
  # Takes data for one plot and the length of a sampling period. Returns histogram of 
  #   time to event with title: name and returns lambda
  
  # Create a vector of the first full day for each camera
  # inputs: data, sampling period length, time that sampling period starts
  # output: vector of date/times from first full day at sampling period starttime to 
  #       last full day at that time
  source("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Image Analysis/Time to event/Time to first event/sampling_start_fn.R")
  st <- sampling_start_fn(data = data, period = period, starttime = starttime)
  
  # Call first_fn function over each s (sampling period start dates)
  #    to create a list of the time to first event for each camera in each sampling period
  source("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Image Analysis/Time to event/Time to first event/first_fn.R")
  first <- lapply(st, first_fn, data = data, period = period)
  
  ###########################
  # Graph time to first event
  ###########################
  
  # Squash the list into an array
  u <- unlist(first)
  res <- tapply(u, sub("\\d+$", "", names(u)), unname)
  
  # Plot time to first event
  hist(as.numeric(res$time), prob = T, breaks = 4,
       xlab = "Time to first event",
       ylab = "Probability Density",
       main = name)
  t <- hist(as.numeric(res$time), prob = T, breaks = 4)
  
  # Get lambda
  avgtime <- mean(as.numeric(res$time), na.rm = T) # average time to event, = 1/lambda
  lambda <- 1/avgtime # average number of events per unit time
  
  # Throw on the exponential curve
  x <- 1:max(t$breaks) - 1
  y <- dexp(x, lambda)
  lines(y~x, col = "red")
  
  return(c("lambda" = lambda, 
           "1/lambda" = 1/lambda,
           "expected variance" = 1/(lambda^2), 
           "observed variance" = var(res$time),
           first))

}
#########################################################################
# Load pilot data
data <- read.csv("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/2014 data/2014data.csv",
                 as.is = T)
data2 <- group_by(data, site)

timetoevent.fn(data = data[data$site == "BH", ], period = 7, name = "BH", starttime = "12:00:00")


# dc <- dc.fn()
# bs <- bs.fn()
# 
# s <- s.fn()
# s$present <- s$elk.present
# s$present[s$present == "Elk present" | s$present == "Elk event"] <- "elk present"
# 
# ig <- ig.fn()
# ig$present <- ig$elk.present
# ig$present[ig$present == "Elk present"] <- "elk present"
# 
# # Run it
# timetoevent.fn(data = dc, period = 7, name = "Deer Canyon")
# timetoevent.fn(data = bs, period = 7, name = "Boulder Spring")
# timetoevent.fn(data = s, period = 7, name = "Swinnerton")
# timetoevent.fn(data = ig, period = 7, name = "Italian Gulch")

