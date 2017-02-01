  # Create encounter histories
  # Anna Moeller
  # 5/25/2016
  
  ideal_fn <- function(startend, starthour, endhour, by_t){
    # Make an "ideal" dataset, all cameras all days.
    ideal.ls <- list()
    for(i in 1:length(startend$cam)){
      # Have to specify time zone so it doesn't account for Daylight Savings Time
      start <- as.POSIXct(paste(startend$plot.start[i], starthour, sep = " "), tz = "GMT")
      end <- as.POSIXct(paste(startend$plot.end[i], endhour, sep = " "), tz = "GMT")
      ideal.ls[[i]] <- expand.grid(
        cam = startend$cam[i],
        ideal = seq(start, end, by = by_t))
      ideal.ls[[i]]$cam <- as.character(ideal.ls[[i]]$cam)
      ideal.ls[[i]]$ideal.date <- as.Date(ideal.ls[[i]]$ideal)
    }
    ideal.df <- do.call(bind_rows, ideal.ls)
  }
  
  eh_fn <- function(data, starthour = "00:00:00", endhour = "23:00:00", 
                 by_t = "day", datelim = NULL, animal.eh = T){
    # Creates an encounter history. Either animal EH or camera effort EH
    # Takes: picture data with SourceFile, cam, dateLST, opstate, elkpresent, plot.start, plot.end
    # animal.eh = F will create an eh for whether the camera is operating
    #   animal.eh = T will create an animal eh
    # by_t = "day" & animal.eh = F will create a camera.op eh where the camera
    #   is open the whole day if there is at least one "normal" opstate photo that day 
    # by_t = "hour" & animal.eh = F will create a camera.op eh where the camera
    #   is assumed open if there is at least one "normal" photo that day
    #   UNLESS there is a censor for at least 30 minutes that hour OR there is
    #   only one photo that hour, and it is a censor.
    # Assume open if SourceFile exists but we haven't gone through the pictures yet
    
    # If they didn't call a datelim, calculate it from the data
    # Second day through second to last day is the default
    if(is.null(datelim)){
      startend <- data %>%
        group_by(cam) %>%
        summarise(plot.start = min(plot.start + 1),
                  plot.end = min(plot.end) - 1)
    } else {
      startend <- data.frame(cam = unique(data$cam),
                            plot.start = datelim[1],
                            plot.end = datelim[2])
    }
    
    # Create an ideal d.f. from between the datelim dates
    ideal.df <- ideal_fn(startend, starthour, endhour, by_t)
    
    # Then join it with the picture metadata
    if(grepl("day", by_t)){
      
      pics2 <- select(data, cam, dateLST, SourceFile, opstate, elkpresent) %>%
        
        # Join with ideal.df to find which days had a photo
        left_join(ideal.df, ., by = c("cam" = "cam", "ideal.date" = "dateLST"))
    
    } else if(grepl("hour", by_t)){
      
      pics2 <- select(data, cam, timeLST, SourceFile, opstate, elkpresent) %>%
        
        # Round pictures down to the hour. 
        mutate(roundtime = as.POSIXct(format(timeLST, format = "%Y-%m-%d %H:00:00"), 
                                      tz = "GMT")) %>%
        # Join with ideal.df to find which hours had a photo
        left_join(ideal.df, ., by = c("cam" = "cam", "ideal" = "roundtime"))
    }
    
    # To make a camera encounter history
    if(animal.eh == F){
      # Make an "Open" column. 
      # Whole day is open if there is at least one "normal" photo that day 
      pics2 <- group_by(pics2, cam, ideal.date) %>%
        mutate(open = any(!is.na(SourceFile) & (opstate == "normal"))) %>%
        ungroup(.)
      
      eh <- group_by(pics2, cam, ideal.date) %>%
        summarise(open = any(!is.na(SourceFile) & (opstate == "normal")))
      
      #If we did it by hour, take it a step further
      if(grepl("hour", by_t)){
        # Make each hour closed if >50% of the hour is not normal
        # It really hates NAs, so we'll split it up then paste it back together
        tbr <- filter(pics2, is.na(SourceFile)) %>%
          select(cam, ideal, open)
        eh <- filter(pics2, !is.na(SourceFile)) %>%
          select(cam, ideal, ideal.date, timeLST, open, opstate, SourceFile) %>%
          group_by(cam, ideal) %>%
          mutate(has.cen = any(opstate != "normal"),
                 has.norm = any(opstate == "normal"),
                 
                 # If all the pictures in that hour are censor, the whole hour is "closed"
                 open = replace(open, has.cen == T & has.norm == F, F),
                 
                 # Figure out how long each censor lasts
                 timelag = difftime(timeLST, lag(timeLST), units = "secs"),
                 cen.len = sum(timelag[which(opstate != "normal") + 1], na.rm = T),
                 
                 # If cen.len > 30 minutes, trap is closed for that hour
                 open = replace(open, cen.len > 30*60, F)) %>%
          
          # Shrink it down and stick it back together
          summarise(open = ifelse(sum(open == 0), F, T)) %>%
          bind_rows(., tbr) %>%
          arrange(cam, ideal)
      }
    } else { 
      # To make an animal encounter history by day or by hour
      if(grepl("day", by_t)){
        eh <- group_by(pics2, cam, ideal.date) %>%
          select(-ideal) %>%
          summarise(seen = ifelse(any(elkpresent == T), T, F)) %>%
          mutate(seen = replace(seen, is.na(seen), F))
                    
      } else if(grepl("hour", by_t)){
        eh <- group_by(pics2, cam, ideal) %>%
          select(-ideal.date) %>%
          summarise(seen = ifelse(any(elkpresent == T), T, F)) %>%
          mutate(seen = replace(seen, is.na(seen), F))
      }
    }
    return(eh)
  }