# Motion vs. time-lapse event analysis
# Anna Moeller 
# 5/21/2015

# Load packages
library(dplyr)
library(lubridate)

# Figure out which events weren't detected by M
source("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Image Analysis/event_ID_fn.R")
elk <- eventID_fn()

bvh <- elk[elk$site == "Beaverhead Reg. 6", ]
bvh2 <- group_by(bvh, eventID) %>%
  summarise(m = sum(trig_type == "M"),
            t = sum(trig_type == "T"))

missed <- bvh2$eventID[bvh2$m == 0 & bvh2$t != 0] # 24/119 events (26/419 including Panhandle)

# Pull out T pics every 10 min instead of every 5
tt <- elk[elk$trig_type == "T", ]
tt$min <-  minute(tt$timeLST)




head(tt[, c("cam", "timeLST", "min")])
