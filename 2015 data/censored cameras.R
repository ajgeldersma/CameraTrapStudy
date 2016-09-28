  # Censored cameras 
  # Anna Moeller
  # 9/2/2016
  
  # Load packages
  source("GitHub/packages.R")
  
  # Load data
  load("C:/Users/anna.moeller/Documents/GitHub/CameraTrapStudy/2015 data/pics.wide20160804.RData")

  # Add an event ID to every sighting of an elk
  source("GitHub/CameraTrapStudy/Image Analysis/eventID_fn.R")
  
  # Make an elk dataframe that is smaller to work with and add eventID
  beav <- select(pics, site, plot, cam, timeLST, dateLST, opstate, trigger, viewer, 
                    DateProcessed, uniquemark, comment, SourceFile, 
                    grep("elk", names(pics), ignore.case = T)) %>%
    do(eventID_fn(data = ., species = "elk", cutoff = 30)) %>%
    filter(site == "Beaverhead") %>%
    group_by(cam)

  # Number of cameras that never got an elk = 19
  allmonths <- beav %>%
    summarise(elk = any(elkpresent == T),
              plot = first(plot))
  length(which(allmonths$elk == F))
  # 19 cameras never got an elk
  table(allmonths$plot[allmonths$elk == F])
  
  # Number of January censors = 32
  jan <- beav %>%
    filter(month(dateLST) == 1) %>%
    summarise(elk = any(elkpresent == T),
              plot = first(plot))
  length(which(jan$elk == F))
  # 32 cameras didn't get an elk in January
  table(jan$plot[jan$elk == F])
  
  # Number of Jan/Feb censors = 29
  janfeb <- beav %>%
    filter(month(dateLST) %in% c(1, 2)) %>%
    summarise(elk = any(elkpresent == T),
              plot = first(plot))
  length(which(janfeb$elk == F))
  # 29 cameras
  
  # Number of Jan/Feb/Mar censors = 20
  jfm <- beav %>%
    filter(month(dateLST) %in% c(1:3)) %>%
    summarise(elk = any(elkpresent == T),
              plot = first(plot))
  length(which(jfm$elk == F))
  # 20 cameras