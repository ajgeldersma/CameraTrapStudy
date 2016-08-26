# first_fn
# Anna Moeller
# 7/6/2015

# Create list of time-to-first-event for each camera in each sampling period
first_fn <- function(data, start, period){
  # start and end are the dates for each sampling period
  end <- start + days(period) # days is a lubridate function
  out <- filter(data, 
                timeLST >= start & timeLST < end & present == "elk present") %>%
    group_by(cam) %>%
    summarise(first.event = min(timeLST),
              site = min(site),
              period_length = period,
              period_start = as.POSIXct(start),
              period_end = as.POSIXct(end)) %>%
    mutate(time = difftime(first.event, start, units = "hours"))
  return(out)
}
