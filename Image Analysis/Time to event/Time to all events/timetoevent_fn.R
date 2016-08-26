# timetoevent_fn
# Anna Moeller
# 7/6/2015

# Create a function that takes data, startdate, length of sampling period, returns 
#   list of 1st events and time to event for all cameras in all sampling periods
timetoevent_fn <- function(data, start, period){
  end <- start + days(period) # days is a lubridate function
  out <- filter(data, 
                timeLST >= start & timeLST < end &
                present == "elk present") %>%
    group_by(cam) %>%
    summarise(first.event = min(timeLST)) %>%
    mutate(time = difftime(first.event, start, units = "hours"))
  return(out)
}