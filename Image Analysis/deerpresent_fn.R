  # Make the deerpresent column work for different species
  # Anna Moeller
  # 7/20/2017
  
  deerpresent_fn <- function(data) {
    # Takes a dataframe with columns: counts of MD (by class), counts of WTD (by class), deerpresent
    # Makes 2 columns: mdpresent, wtdpresent
    # When deerpresent == T, it puts in the last detected species
    # conservative: it will ignore counts if deerpresent == F
    # If both species are present, both columns will say T
    # For a new deerpresent == T with no count, both columns will say F
    mdcols <- grep("md", names(data), ignore.case = T)
    wtdcols <- grep("wtd", names(data), ignore.case = T)
    tmp <- data %>%
      mutate(md = apply(.[, mdcols], 1, sum, na.rm = T),
             wtd = apply(.[, wtdcols], 1, sum, na.rm = T),
             mdpresent = ifelse(md > 0 & deerpresent == T, T, ifelse(deerpresent == T & md == 0 & wtd == 0, NA, F)), 
             wtdpresent = ifelse(wtd > 0 & deerpresent == T, T, ifelse(deerpresent == T & md == 0 & wtd == 0, NA, F)),
             mdpresent = zoo::na.locf(mdpresent),
             wtdpresent = zoo::na.locf(wtdpresent)
        ) %>%
      select(-md, -wtd, -deerpresent)
    
    
    # tmp <- data %>%
    #   mutate(md = apply(.[, mdcols], 1, sum, na.rm = T),
    #          wtd = apply(.[, wtdcols], 1, sum, na.rm = T),
    #          deerspp = ifelse(md > 0, "md", ifelse(wtd > 0, "wtd", ifelse(deerpresent == F, "none", NA)) ),
    #          deerspp = zoo::na.locf(deerspp),
    #          mdpresent = ifelse(deerspp == "md", T, F), 
    #          wtdpresent = ifelse(deerspp == "wtd", T, F)
    #          ) %>%
    #   select(-md, -wtd, -deerpresent, -deerspp)

    # Give a warning if MD and WTD are in the same picture (possible, but maybe a mistake)
    ww <- length(which(tmp$mdpresent == T & tmp$wtdpresent == T))
    warning(ww > 0, paste0(" MD and WTD present together in ", ww, " pictures"))

    return(tmp)
  }

  # # Dataframe to test it out
  # data <- data.frame(MDbuck = c(0,0,0,1,0,0,1,1),
  #                    MDantlerless = c(0,0,0,1,0,0,0,0),
  #                    WTDbuck = c(1,0,0,0,0,0,0,1),
  #                    WTDfawn = c(0,0,1,0,0,0,0,0),
  #                    deerpresent = c(T,T,T,T,F,T,F,T) )

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
  
  

