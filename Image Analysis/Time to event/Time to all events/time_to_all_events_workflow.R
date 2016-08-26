# Time to first event
# Anna Moeller 
# 7/1/2015

# Load packages
library(dplyr)
library(lubridate)

# Load Deer Canyon
source("C:/Users/anna.moeller/Dropbox/R Scripts/Image Analysis/import Deer Canyon.R")
dc <- dc.fn(x)

# Load Boulder Spring
source("C:/Users/anna.moeller/Dropbox/R Scripts/Image Analysis/import Boulder Spring.R")
bs <- bs.fn(x)

# Add an event_ID column
source("C:/Users/anna.moeller/Dropbox/R Scripts/Image Analysis/event_ID_fn.R")






##########################
# Find time to all events
##########################

# Length of the sampling period in days
period <- 7

# Beginning and end
datestart <- as.POSIXct(paste(min(bs$dateLST) + 1, "12:00:00"), tz = "MST")
dateend <- as.POSIXct(paste(max(bs$dateLST) - 1, "12:00:00"), tz = "MST")

# Call first_fn function over all s to create a list of the time to first event 
#    for each camera in each sampling period
source("C:/Users/anna.moeller/Dropbox/R Scripts/Image Analysis/Time to first event/first_fn.R")
first <- lapply(s, first_fn, data = bs, period = period)

#########################
# Let's take a look at it
#########################

# Number of sampling periods
length(first)

# Number of cameras that have an event in each sampling period
n <- sapply(first, dim)[1, ]

# Number of cameras with no event in each sampling period
ncam <- length(unique(bs$cam))
censor <- ncam - n

###############
# Let's plot it
###############

# Squash the list into an array
u <- unlist(first)
res <- tapply(u, sub("\\d+$", "", names(u)), unname)

# Plot time to first event
hist(as.numeric(res$time), prob = T, breaks = 4)
t <- hist(as.numeric(res$time), prob = T, breaks = 4)

# Get lambda
avgtime <- mean(as.numeric(res$time), na.rm = T) # average time to event, = 1/lambda
lambda <- 1/avgtime # average number of events per unit time

# Throw on the exponential curve
x <- 1:max(t$breaks) - 1
y <- dexp(x, lambda)
lines(y~x, col = "red")

################################################################################