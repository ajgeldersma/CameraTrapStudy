  # Make the deerpresent column work for different species
  # Anna Moeller
  # 8/9/2016
  
  # Separate out deerpresent
  deerpresent_fn <- function(data) {
    mdcols <- grep("md", names(data), ignore.case = T)
    wtdcols <- grep("wtd", names(data), ignore.case = T)
    data <- mutate(data, 
                   md = apply(data[, mdcols], 1, sum, na.rm = T),
                   wtd = apply(data[, wtdcols], 1, sum, na.rm = T),
                   spp = ifelse(wtd > 0, "wtd", ifelse(md > 0, "md", NA)),
                   spp = replace(spp, 1, "placeholder"), # have to start with a placeholder for zoo
                   spp = zoo::na.locf(spp), # fill in NAs from the last value
                   mdpresent = ifelse(spp == "md" & deerpresent == T, T, F),
                   wtdpresent = ifelse(spp == "wtd" & deerpresent == T, T, F)) %>%
      select(-md, -wtd, -spp, -deerpresent)
  }
  
  

