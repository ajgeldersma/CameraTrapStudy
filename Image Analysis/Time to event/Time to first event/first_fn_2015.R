  # first_fn
  # Anna Moeller
  # 7/6/2015
  
  # Create list of time-to-first-event for each camera in each sampling period
  first_fn <- function(data, start, starttime, period, species){
  
    # Make sure if it's for deer that the data have been reshaped
    if(species == "md" | species == "wtd"){
      stopifnot(!is.null(data$mdpresent) | !is.null(data$wtdpresent))
    }
    
    # Find the appropriate "present" column 
    prescol <- grep(species, names(data))
    stopifnot(!is.null(data[, prescol]))
    
    # Find the timezone
    timz <- tz(data$timeLST)
   
    # Create a dataframe with all the days you're looking for
    end <- start + days(period)
    
    # Find the start time numerically
    tmp <- as.POSIXct(starttime, format = "%H:%M:%S")
    h <- hour(tmp)
    m <- minute(tmp)
    s <- second(tmp)
    add <- h*60*60 + m*60 + s
    
    # Subtract the starttime from the data so the day will line up with the ideal day 
    cht <- data %>%
      mutate(cheat = as.Date(timeLST - add))
  
    # Create an ideal dataframe
    idf <- data.frame(idealtime = seq(min(start), max(end), by = "day")) %>%
      mutate(idealdate = as.Date(idealtime),
             per = 0,
             per = replace(per, idealdate %in% as.Date(start), 1),
             per = cumsum(per))
    
    # Repeat this dataframe for each camera
    tmp <- expand.grid(idealtime = idf$idealtime, cam = unique(data$cam)) %>%
      mutate(cam = as.character(cam)) %>%
      left_join(., idf, by = c("idealtime" = "idealtime")) %>%
      
      # Join this ideal df with the data, to align all the pictures with a event period
      ### You get an NA here if there were no photos on that day
      left_join(., cht, by = c("cam" = "cam", "idealdate" = "cheat")) %>%
   
      # Add a column: T when there is at least one detection in that period, F otherwise
      group_by(per, cam) %>%
      mutate(willwork = any(elkpresent == T, na.rm = T)) 
    
    # Take out the periods with no pictures for later
    # Because it gets cranky if EVERYTHING is NA
    forlater <- tmp %>% 
      filter(willwork == F) %>%
      summarise(pictureLST = NA,
                start = min(idealtime))
    
    # find the first picture for every camera for every event period
    ttfe <- tmp %>%
      filter(willwork == T) %>%
      summarise(pictureLST = min(timeLST[elkpresent == T], na.rm = T),
                start = min(idealtime)) %>%
      bind_rows(., forlater) %>%
      mutate(end = start + days(period),
             pictime = as.numeric(difftime(pictureLST, start, units = "hours"))) %>%
      arrange(cam, per, start)
                     
    return(ttfe)
  }
  
