# Call time to event on 2014 pilot data
# Anna Moeller

# Load pilot data
pics <- read.csv("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/2014 data/2014data.csv",
                 as.is = T)

time <- timetoevent.fn(data = pics, period = 7, starttime = "12:00:00")

# Glue time elements together into a data.frame
time2 <- lapply(time, as.data.frame) 
for(i in 1:length(time2)){
  time2[[i]]$period <- i
}
time2014 <- do.call(rbind, time2)

# Save this 
# save(time2014, file = "C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/2014 data/2014timetoevent_per7.RData")
# save(time2014, file = "C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/2014 data/2014timetoevent_per3.RData")

