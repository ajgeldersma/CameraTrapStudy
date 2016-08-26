# sampling_start_fn
# Anna Moeller
# 7/6/2015


sampling_start_fn <- function(data, period, datelim = NULL, starttime){
  # Create a vector of sampling period start dates 
  # If datelim == NULL, the function will use the first full day from the data
  
  # Make sure datelim is the right dimension
  stopifnot(is.null(datelim) | length(datelim) == 2)

  # Make sure dateLST and timeLST are the right class
  stopifnot(class(data$dateLST) == "Date" & class(data$timeLST) == c("POSIXct", "POSIXt"))
  
  # Figure out what time zone we're dealing with
  timz <- tz(data$timeLST)
  
  if(is.null(datelim)) {
    
    # Name of first full day at starttime
    datestart <- as.POSIXct(paste(min(data$dateLST) + 1, starttime), tz = timz)
    
    # Name of last full day at starttime
    dateend <- as.POSIXct(paste(max(data$dateLST) - 1, starttime), tz = timz)  
    
  } else {
    datestart <- as.POSIXct(paste(datelim[1], starttime), tz = timz)
    dateend <- as.POSIXct(paste(datelim[2], starttime), tz = timz)
  }
  
  # Vector of all the start days
  s <- seq(from = datestart, to = dateend, by = paste(period, "days"))
  
  # Take out the last day in s, because it will never be a full sampling period.
  s <- s[1:(length(s) - 1)]
  
  # Output
  return(s)
}