# check_noon_fn
# Anna Moeller
# 3/25/2016

check_noon_fn <- function(data, access){
  # Takes a dataframe for a single camera and
  #   a dataframe with the start and end dates
  # Check for a noon picture every day (excluding first and last)

  acc2 <- access[access$camID == data$cam[1], ]
  
  # Find the start and end date from what we already know
  if(length(unique(acc2$DeploymentID)) == 1){
    date.start <- as.Date(min(acc2$op.start), format = "%Y-%m-%d")
    date.end <- as.Date(min(acc2$op.end), format = "%Y-%m-%d")
  } else {
    # Something else here that will work for the multiple deployments 
  }

  # Stop if any incoming photo date is NA
  stopifnot(!any(is.na(data$timeLST))) 
  
  # Make sure the photo dates are formatted correctly
  data$dateLST <- as.Date(data$dateLST, format = "%Y-%m-%d")
  data$timeLST <- as.POSIXct(data$timeLST)
  
  # Make a vector of all the full days of this camera (unless it had no full days),
  #   that I want it to work for
  if(date.start + 1 < date.end - 1){
    s <- seq(from = date.start, to = date.end, by = "days") 
    s2 <- as.POSIXct(paste(s, "12:00:00"), tz = "MST")
    
    # See if there is a record somewhere in the camera that matches s2
    checknoon <- sapply(s2, function(x){
      any(data$timeLST == x)
    })
    
    if(any(checknoon == F)){
      out <- paste("noon fail on", s2[checknoon == F], "for camera", min(data$cam))
    } else {
      out <- paste("noon success for camera", min(data$cam))
    }
  } else {
    out <- paste(min(data$cam), "had no full days")
  }
  return(out)
}

############

# check_noon_fn <- function(data){
#   # Check for a noon picture every day (excluding first and last day)
#   data$dateLST <- as.Date(data$dateLST)
#   fail <- NULL
#   date.start <- min(data$dateLST) + 1
#   date.end <- max(data$dateLST) - 1
#   s <- seq(from = date.start, to = date.end, by = "days") 
#   s2 <- as.POSIXct(paste(s, "12:00:00"), tz = "MST")
#   stopifnot(!any(is.na(s2))) # Function will stop if any incoming date is NA
#   for (i in 1:length(s2)) {
#     if(!any(data$timeLST == s2[i])){
#       fail[i] <- as.character(s[i])
#     } else {
#       fail[i] <- 9999
#     }
#   }
#   if (all(fail == 9999)) {
#     print("Noon success")
#     return(NULL)
#   } else {
#     fail[fail==9999] <- NA
#     fail2 <- as.Date(fail[!is.na(fail)])
#     print("There are dates without noon pictures")
#   }
#   return(fail2)
# }