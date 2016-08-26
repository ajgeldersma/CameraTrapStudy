# event_time_fn
# Anna Moeller
# 5/19/2015

event_time_fn <- function(data){
# Make new data.frame with event time, duration
# deadspace: first record: end of setup to first "new event", 
#            subsequent records: from "end event" to next "new event"
# duration and deadspace are in seconds
  time <- data %>% 
    filter(!is.na(eventID)) %>%
    group_by(eventID) %>% 
    summarise(start = min(timeLST), 
              end = max(timeLST)) %>% 
    mutate(dur_mins = as.numeric(difftime(end, start, units = "mins")),
          deadsp_days = c(as.numeric(difftime(start[1], pretty$timeLST[min(which(pretty$op.state == ""))], 
                                              units = "days")), 
                          as.numeric(difftime(start[2:length(start)], end[1:(length(start)-1)], units = "days"))))
  return(time)
}