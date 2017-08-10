  # Make the deerpresent column work for different species
  # Anna Moeller
  # 7/20/2017
  
  deerpresent_fn <- function(data) {
    # Takes a dataframe with columns: counts of MD (by class), counts of WTD (by class), deerpresent
    # Looks where the count of MD or WTD > 0 and deerpresent is True. (conservative approach)
    # Makes 2 columns: mdpresent, wtdpresent
    mdcols <- grep("md", names(data), ignore.case = T)
    wtdcols <- grep("wtd", names(data), ignore.case = T)
    tmp <- data %>%
      mutate(md = apply(.[, mdcols], 1, sum, na.rm = T),
             wtd = apply(.[, wtdcols], 1, sum, na.rm = T),
             mdpresent = ifelse(md > 0 & deerpresent == T, T, F),
             wtdpresent = ifelse(wtd > 0 & deerpresent == T, T, F)
      )
    
    # Give a warning if MD and WTD are in the same picture (possible, but maybe a mistake)
    ww <- length(which(tmp$mdpresent == T & tmp$wtdpresent == T))
    warning(ww > 0, paste0(" MD and WTD present together in ", ww, " pictures"))
    
    # I'm going to keep mdpresent and wtdpresent everywhere there is a count and ignore
    #   where there is a count but deerpresent is F. 
    # Give a warning though
    nomd <- length(which(tmp$md > 0 & tmp$deerpresent == F))
    nowtd <- length(which(tmp$wtd > 0 & tmp$deerpresent == F))
    warning(nomd > 0, paste0(" ", nomd, " records ignored because MD counted but deerpresent False"))
    warning(nowtd > 0, paste0(" ", nowtd, " records ignored because WTD counted but deerpresent False"))
    
    tmp2 <- tmp %>%
      select(-md, -wtd, -deerpresent)
    return(tmp2)
  }



  # 8/9/2016
  # # Separate out deerpresent
  # deerpresent_fn <- function(data) {
  #   mdcols <- grep("md", names(data), ignore.case = T)
  #   wtdcols <- grep("wtd", names(data), ignore.case = T)
  #   data <- mutate(data, 
  #                  md = apply(data[, mdcols], 1, sum, na.rm = T),
  #                  wtd = apply(data[, wtdcols], 1, sum, na.rm = T),
  #                  spp = ifelse(wtd > 0, "wtd", ifelse(md > 0, "md", NA)),
  #                  spp = replace(spp, 1, "placeholder"), # have to start with a placeholder for zoo
  #                  spp = zoo::na.locf(spp), # fill in NAs from the last value
  #                  mdpresent = ifelse(spp == "md" & deerpresent == T, T, F),
  #                  wtdpresent = ifelse(spp == "wtd" & deerpresent == T, T, F)) %>%
  #     select(-md, -wtd, -spp, -deerpresent)
  # }
  
  

