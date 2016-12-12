  # Occupancy for MARK
  # Anna Moeller
  # 6/7/2016
  
  # Make an occupancy encounter history for MARK
  
  # Load packages
  library(tidyr)
  library(plyr)
  library(dplyr)
  
  # Load pictures and access database
  load("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/2015 data/pics.wide20160606.RData")
  load("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/2015 data/access.sum.RData")
  
  # Make a dataframe for camera number 1-9
  camnum <- select(access.sum, camID, plot, camnum) %>%
    # Drop AM157 because there were two cameras in this cell and the card got lost
    filter(camnum != 0)
  
  # Create an effort encounter history by week
    # Start by making an eh by day
    source("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Image Analysis/eh_fn.R")
    cam.eh <- eh_fn(pics, access.sum, starthour = "12:00:00", endhour = "12:00:00", 
                    by_t = "day", animal.eh = F)
    
    # Look at the week I'm using and if at least 4 of the days are open, call it open
    effort <- filter(cam.eh, ideal.date >= as.Date("2016-02-01") & 
                       ideal.date <= as.Date("2016-02-08")) %>%
      group_by(cam) %>%
      summarise(test = length(which(open == T))) %>%
      mutate(open = ifelse(test >= 4, T, F)) %>%
      select(-test)
  
  # Create an occupancy encounter history for a single week
  # This gives NAs where there is no "elkpresent" column for the entire week
  #   That includes photos that have not been gone through yet and
  #   photos that don't exist (got deleted/stolen, etc.)
  # This also accounts for censored photos using effort, above
    occ <- filter(pics, dateLST >= as.Date("2016-02-01") & dateLST <= as.Date("2016-02-08")) %>%
      group_by(cam) %>%
      summarise(occupied = any(elkpresent == T),
                plot = min(plot)) %>%
      right_join(., camnum, by = c("cam" = "camID")) %>% # This is to change cam from an ID to a number 1-9
      left_join(., effort, by = c("cam" = "cam")) %>% # This adds the "open" column
      mutate(occupied = replace(occupied, open == F, NA), # If it's closed, occupied is an NA
             occupied = ifelse(is.na(occupied), ".", ifelse(occupied == T, "1", "0"))) %>% 
      select(-plot.x, -cam, -open) %>% 
      rename(plot = plot.y) %>%
      spread(camnum, occupied) %>%
      unite(ch, 2:10, sep = "") # tidyr
  
  # For JAGS
  occ <- filter(pics, dateLST >= as.Date("2016-02-01") & dateLST <= as.Date("2016-02-08")) %>%
    group_by(cam) %>%
    summarise(occupied = any(elkpresent == T),
              plot = min(plot)) %>%
    right_join(., camnum, by = c("cam" = "camID")) %>% # This is to change cam from an ID to a number 1-9
    left_join(., effort, by = c("cam" = "cam")) %>% # This adds the "open" column
    mutate(occupied = replace(occupied, open == F, NA), # If it's closed, occupied is an NA
           occupied = ifelse(occupied == T, 1, ifelse(occupied == F, 0, NA))) %>% 
    select(-plot.x, -cam, -open) %>% 
    rename(plot = plot.y) %>%
    spread(camnum, occupied) 
  
  # BH03, BH05, BH11, BH12 are done for this week. 
  #   BH03 only has 8 cameras out (AM999 filled in)
  #   BH11 only has 8 b/c AM62 didn't work on this day
  #   BH05 only has 8 b/c AM33 is missing the entire Trip 2 (1/18-3/15)
  
  # occupied = NA, open = T, the photos exist but they haven't been looked at
  #   This makes sense because I assumed they were all open if they haven't been looked at.
  # occupied = NA, open = F, the photos don't exist
  # occupied = F, open = T, the camera was open for the week but no elk were detected
  # occupied = T, open = T, the camera was open for the week and elk were detected
  # occupied = F, open = F, the pictures exist but were censored
  # occupied = T, open = F, the pictures exist but were censored but animals were seen anyway
  # Neither of the last two exist yet
  
  
  
  #######################################################################
  # # Specify which day I want to start my week on.
  # # summarise open if at least 4 of the days are open
  # dvec <- as.Date("2016-02-01") + 0:length(cam.eh$cam)
  # dweek <- as.numeric(dvec-dvec[1]) %/% 7
  # dweek[1:21]
  
  #   mutate(occupied = replace(occupied, 
  #                             cam == "AM999" | cam == "AM62" | cam == "AM33", F)) %>%
  #   filter(plot == "BH03" | plot == "BH05" | plot == "BH11" | plot == "BH12")
    ## This all needs to be adjusted when all the photos are analyzed
  
  # table(working$plot[!is.na(working$occupied)])
  
  
  
  
    