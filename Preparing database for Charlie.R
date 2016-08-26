  # Preparing database for Charlie
  
  # Load packages
  source("GitHub/packages.R")
  
  # Bring in databases
  load("GitHub/Camera-trap-study/2015 data/pics.wide20160804.RData")
  
  # Preprocess the deer data for eventID function
  source("GitHub/Camera-trap-study/Image Analysis/deerpresent_fn.R")
  mddata <- deerpresent_fn(pics)
  
  # Add an eventID to mule deer
  source("GitHub/Camera-trap-study/Image Analysis/eventID_fn.R")
  char <- eventID_fn(mddata, species = "md", cutoff = 30) %>%
    
    # Select only mule deer columns
    select(site, plot, cam, timeLST, dateLST, mdpresent, MDbuck, MDantlerless, 
           MDfawn, MDunkn, eventID, uniquemark, viewer, opstate, 
           easting, northing, SourceFile)
    
    # Only plots 2 and 3
    #filter(plot %in% c("BH02", "BH03"))
    
  #save(char, file = "L:/Camera_MDdata_Anna/muledeercameras_all.RData")
    
  # summarise group size
  summ <- group_by(char, eventID) %>%
    summarise(site = min(site),
              cam = min(cam),
              eventstart = min(timeLST),
              buck = sum(MDbuck),
              antlerless = sum(MDantlerless),
              fawn = sum(MDfawn),
              unkn = sum(MDunkn)) %>%
    mutate(total = buck + antlerless + fawn + unkn)

  # Look at the distribution across cameras
  table(summ$cam[summ$total == 0])
  
###########################################################
  # Elk database for January
  
  # For eventID 
  source("GitHub/Camera-trap-study/Image Analysis/eventID_fn.R")

  elk <- pics %>%
    select(site, plot, cam, timeLST, dateLST, grep("elk", names(pics), ignore.case = T), 
            uniquemark, viewer, opstate, easting, northing, SourceFile) %>%
    filter(dateLST >= as.Date("2016-01-01") & dateLST <= as.Date("2016-01-31"),
           plot %in% c("BH01", "BH03", "BH04", "BH06", "BH07", "BH08", "BH09")) %>%
    do(eventID_fn(., species = "elk", cutoff = 30))
  
  # Save it
  save(elk, file = "GitHub/Camera-trap-study/Elk db Jan for Charlie.RData")
      
  