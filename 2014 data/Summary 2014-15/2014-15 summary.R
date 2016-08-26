# 2014 data breakdown
# Anna Moeller
# 3/24/2016

# Load packages
library(lubridate)
library(dplyr)

# Load 2014 time to event data
# The object is called time2014
load("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/2014 data/2014timetoevent_per7.RData")
per7 <- time2014

load("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/2014 data/2014timetoevent_per3.RData")
per3 <- time2014

# Make a histogram for time-to-event for each site
per7$time <- as.numeric(per7$time)
per3$time <- as.numeric(per3$time)

par(mfrow = c(2,3))
hist(per7$time[per7$site == "BS"], xlab = "Time to First Capture", main = "BS", breaks = 4)
hist(per7$time[per7$site == "DC"], xlab = "Time to First Capture", main = "DC", breaks = 4)
hist(per7$time[per7$site == "KC"], xlab = "Time to First Capture", main = "KC")
hist(per7$time[per7$site == "IG"], xlab = "Time to First Capture", main = "IG", breaks = 4)
hist(per7$time[per7$site == "S"], xlab = "Time to First Capture", main = "S", breaks = 4)

par(mfrow = c(2,3))
hist(per3$time[per3$site == "BS"], xlab = "Time to First Capture", main = "BS", breaks = 4)
hist(per3$time[per3$site == "DC"], xlab = "Time to First Capture", main = "DC", breaks = 4)
hist(per3$time[per3$site == "KC"], xlab = "Time to First Capture", main = "KC")
hist(per3$time[per3$site == "IG"], xlab = "Time to First Capture", main = "IG", breaks = 4)
hist(per3$time[per3$site == "S"], xlab = "Time to First Capture", main = "S", breaks = 4)

# Summarize what was caught at each site
pics <- read.csv("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/2014 data/2014data.csv", as.is = T)
source("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Image Analysis/event_ID_fn.R")
out <- event_ID_fn(pics, cutoff = 30)
#write.csv(out, "C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/2014 data/2014data_eventID.csv")

# 312 unique elk events (30 min minimum) (without 0s)
length(unique(out$eventID))-1

# Elk events by site (subtract 1 to not count event = 0)
summ <- group_by(out, site) %>%
  summarise(elk_events = length(unique(eventID))-1)

# Check if every camera took a picture at noon
source("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Image Analysis/check_noon_fn.R")

data <- lapply(unique(out$cam), function(x){
  out[out$cam == x, ]  
})

noon <- lapply(data, check_noon_fn)
noon[grep(c("fail|had no"), noon)]

### Noon failures in AM03, AM10, which malfunctioned. 
# Also noon failure for AM09, why????? Programmed wrong? 

### Do something with cameras that malfunctioned
# AM03 and AM10 malfunctioned or were messed with
# AM09 may not have been programmed right. 
# AM07 only worked for 7 days...
# AM43 only worked for 15 days. There is a lot of variation in the number of op. days

# Also look up which cameras had their cards fill up. 
# Match the start and end date with their access db dates
# Do something with tilted, etc...

# Summarize all the animals we got on film
out2 <- group_by(out, 
                 cam) %>%
  summarise(site = min(site),
            npics = length(file),
            elk_events = length(unique(eventID))-1,
            elkcow = sum(elkcow),
            elkbull = sum(elkbull),
            elkcalf = sum(elkcalf),
            elkunkn = sum(elkunkn),
            md = sum(md),
            ph = sum(ph),
            wtd = sum(wtd),
            coyote = sum(coyote),
            moose = sum(moose),
            lion = sum(lion),
            human = sum(human),
            marked = sum(grepl("elk|eartag", collaryn, ignore.case = T)),
            days_on = difftime(max(timeLST), min(timeLST)),
            photos_per_day = npics/as.numeric(days_on),
            normal_op_days = length(unique(dateLST[opstate == "operating"])),
            tilted_days = length(unique(dateLST[opstate == "tilted"])),
            problem = ifelse(any(opstate == "malfunction"|opstate == "tampered with"|
                                 opstate == "wrong direction"), T, F))

############## Comparing with Access. Not really useful #####################

# Read in data from Access
#x <- read.csv("C:/Users/anna.moeller/Documents/Camera Trap Study/Cameras/Deployment.csv")
deploy <- read.csv("C:/Users/anna.moeller/Documents/Camera Trap Study/Cameras/2014 Deployment long.csv", as.is = T)

# Make it prettier
deploy <- rename(deploy,
                 ID = Deployment.ID,
                 cam = Camera.ID,
                 batt = Batteries,
                 sd = SD.Card,
                 cable = Cable.Lock,
                 key = Lock.Key,
                 site = Location,
                 deploy = Deployment.Date,
                 redeploy = Redeployment.Date,
                 batt_life = Battery.Life.1,
                 pics = Pictures.Taken.1,
                 takedown = Take.Down.Date,
                 censor = Censor.Date) %>%
  mutate(deploy = as.Date(as.character(deploy), format = "%m/%d/%Y"),
         redeploy = as.Date(as.character(redeploy), format = "%m/%d/%Y"),
         takedown = as.Date(as.character(takedown), format = "%m/%d/%Y"),
         censor = as.Date(as.character(censor), format = "%m/%d/%Y"),
         batt_life = as.numeric(sub("%", "", batt_life))/100,
         region = ifelse(site == "Swinnerton"|site == "Italian Gulch", "Panhandle", 
                         ifelse(site == "Kenney Creek"|site == "Buckhorn", 
                                "Beaverhead Reg. 7", "Beaverhead Reg. 6")))

# Find the total number of pictures taken
sum(deploy$pics, na.rm = T) #345694
# Vs. method above sum(out2$npics) = 348852 # Not all records match up. This is because
#   I have some blanks in access where the camera died and I couldn't see how many pics 
#   there were. Also, if I visited twice but left the card, I need to not add the number of pics

deploy2 <- group_by(deploy,
                   cam) %>%
  summarise(npics = sum(pics, na.rm = T))

# These are the cameras where records don't match up
xx <- out2$cam[out2$npics != deploy2$npics]

# I really don't need to deal with this stuff. Let's just say Timelapse is right. 
###################################################################################

# Do expo_model on my real data
# (in expo_model.R)



# par(mfrow = c(1,2),
#     oma = c(0,0,2,0))
# hist(per7$time[per7$site == "S"], xlab = "Time to First Capture", 
#      main = "Panhandle Site (n = 47)", prob = T, breaks = 4)
# hist(per3$time[per7$site == "DC"], xlab = "Time to First Capture", 
#      main = "Beaverhead Site (n = 22)", prob = T, breaks = 4)
# title(main = "Time to Elk Event 2014", outer = T)