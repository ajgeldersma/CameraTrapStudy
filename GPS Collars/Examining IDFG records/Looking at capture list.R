# Looking at the capture records from Mike
# Anna Moeller 
# 5/19/2015

# Load packages
library(dplyr)

# Read in data
#  This is the most recent version sent to me by Mike on 3/30/15
capture <- read.csv("C:/Users/anna.moeller/Documents/IDFG/Capture 2014-15/Copy of Recent Elk Captures from Mike2.0.csv")
cap2 <- filter(capture,
               !is.na(CaptureID)) %>% 
        mutate(date = as.Date(as.character(Capture_Date), format = "%m/%d/%Y"))

# Panhandle
panh <- filter(cap2, 
               GMU == 4 | GMU == 6 | GMU == 7 | GMU == 9,
               date > as.Date("12/1/2014", format = "%m/%d/%Y"))
pan2 <- group_by(panh,
                 Animal_ID) %>%
        summarise(GMU, Age_Class)
# 
  
# Beaverhead
beav <- filter(cap2, 
               GMU == "30" |  GMU == "30A" | GMU == "58" | GMU == "59A" | GMU == "59",
               date > as.Date("12/1/2014", format = "%m/%d/%Y"))
# 56 captures, none in 58 or 59. This is still true. 

summary(panh$Age_Class)
summary(beav$Age_Class)
