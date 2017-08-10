  # Importing 2015-16 data
  # Anna Moeller
  # 3/30/2016
  
  # Load packages
  source("GitHub/packages.R")
  
##############################################
  # 1. List all the directories where photos are
  
  # alldirs <- list.dirs("F:/Camera Trap Photos/2015-16")
  # trips <- alldirs[grepl("RECNX", alldirs)]
  # trips <- trips[!grepl("Backups$|Kellogg", trips)]
  # save(trips, file = "GitHub/CameraTrapStudy/2015 data/trips.RData")
  load("GitHub/CameraTrapStudy/2015 data/trips.RData")
  
###########################################
  # Bring in abbreviated access database
  load("C:/Users/anna.moeller/Documents/GitHub/CameraTrapStudy/2015 data/access.sum.RData")
  access <- access.sum
  
#############################################
  # 3. Make sure every file has a metadata file
  
  # 3.1 Check if there is metadata for all photos. 
  # List all the trips folders where there is no metadata saved
  # files <- lapply(trips, list.files, pattern = "Metadata.csv")
  # nometa <- trips[which(lapply(files, length) == 0)]
  
  # 3.2 If there is no metadata, run exif on these folders to get it
  # library(exifr)
  # source("C:/Users/anna.moeller/Documents/GitHub/CameraTrapStudy/2015 data/Exporting metadata.R")
  # lapply(nometa, get_meta_fn)
  
###############################################
  # Copy over timelapse files as they get finished
  source("GitHub/CameraTrapStudy/2015 data/pull.fn.R")
  
  # # Example call (all new cameras):
  # pull.fn(from = "I:/Camera Trap Photos/2015-16",
  #         dest = "F:/Camera Trap Photos/2015-16",
  #         destbackup = paste("F:/Camera Trap Photos/Other/Old Files", Sys.Date(), sep = "/")
  # )
  
  # # Example call (particular camera):
  # pull.fn(from = "G:/Camera Trap Photos/2015-16/St. Joe/AM108/Trip 1/100RECNX",
  #         dest = "F:/Camera Trap Photos/2015-16/St. Joe/AM108/Trip 1/100RECNX",
  #         destbackup = paste("F:/Camera Trap Photos/Other/Old Files", Sys.Date(), 
  #                            "St. Joe/AM108/Trip 1/100RECNX", sep = "/")
  # )
  
  # Example call (multiple specified cameras)
  # xx <- unique(dirname(list.files("G:/Camera Trap Photos/2015-16/St. Joe", full.names = T, recursive = T)))
  # xx <- xx[!grepl("Backups", xx)]
  # from <- xx[7:22]
  # 
  # sapply(from, function(x){
  #   dest <- gsub("^G:", "F:", x)
  #   destbackup <- gsub("2015-16", paste("Other/Old Files", Sys.Date(), sep = "/"), dest)
  #   pull.fn(from = x, dest = dest, destbackup = destbackup)
  # })



####################################
  # 4. Combine metadata and timelapse
  # 4.1 Open each picture folder, attach the timelapse and metadata together
  
  # Crapola. The metadata are different for the different camera models. Still
  #   waiting on ExifTool to come up with a way to do the Ultrafires
  # BLECH. The PC800s are different too. 
  
  # Clean up metadata and Timelapse, and combine together 
  source("C:/Users/anna.moeller/Documents/GitHub/CameraTrapStudy/2015 data/combine metadata and timelapse 2015.R")
  
  # Bind them together and fill in NAs if the TIA file wasn't there
  pics1 <- do.call(bind_rows, lapply(trips[1:50], combo_timelapse3_fn))
  pics2 <- do.call(bind_rows, lapply(trips[50:100], combo_timelapse3_fn))
  pics3 <- do.call(bind_rows, lapply(trips[100:150], combo_timelapse3_fn))
  pics4 <- do.call(bind_rows, lapply(trips[150:200], combo_timelapse3_fn))
  pics5 <- do.call(bind_rows, lapply(trips[200:254], combo_timelapse3_fn))
  pics <- bind_rows(pics1, pics2, pics3, pics4, pics5)
  
  # Add GPS coordinates and operation dates to all the cameras
  pics <- left_join(pics, access, by = c("cam" = "camID")) %>%
    
    # Delete the photo with the wrong UserLabel
    filter(cam != "T01 BIGHORN") %>%
    
    # Delete the X column (couldn't do this earlier because not every camera has this)
    select(-X) %>%
    
    # Rename stupid things
    rename(ElkNubsPeds = Elknubspeds,
           ProngBuck = PronghornBuck,
           ProngFawn = PronghornFawn,
           ProngDoe = PHdoe,
           ProngUnkn = PHunkn) %>%
    
    # Fix AM195 naming (says AM192) and AM76 naming (says AM75)
    mutate(cam = replace(cam, grepl("AM76", SourceFile), "AM76"),
           cam = replace(cam, grepl("AM195", SourceFile), "AM195"),
           plot = replace(plot, grepl("AM195", SourceFile), "BH08")) %>%
    
    # Fix time on AM191 (20 minutes early) and AM151 (totally off)
    # 322 days, 10 hours, 9 minutes behind real time
    mutate(timeLST = replace(timeLST, cam == "AM191", timeLST[cam == "AM191"] + 20*60),
           timeLST = replace(timeLST, cam == "AM151", timeLST[cam == "AM151"] + 322*60*60*24 + 10*60*60 + 9*60),
           timeLST = replace(timeLST, cam == "AM110", timeLST[cam == "AM110"] - 1*60*60*24*365),
           dateLST = replace(dateLST, cam == "AM191", as.Date(timeLST[cam == "AM191"])),
           dateLST = replace(dateLST, cam == "AM151", as.Date(timeLST[cam == "AM151"])),
           dateLST = replace(dateLST, cam == "AM110", as.Date(timeLST[cam == "AM110"]))) %>%
    
    # Change all the sourcefile names to match the trips ("F:")
    mutate(SourceFile = gsub("^[GH]:", "F:", SourceFile))
    
    # Fix column names with bad characters 
    names(pics) <- gsub("\\(|\\)|", "", names(pics))
    names(pics) <- gsub(" ", ".", names(pics))
    
    # Drop the blank column
    #select(-novalue)
    if(!(any(pics$novalue != 0 & !is.na(pics$novalue)))){
      pics$novalue <- NULL
    } else { 
      print(pics$SourceFile[which(pics$novalue != 0)])
    }
  
  #save(pics, file = "C:/Users/anna.moeller/Documents/GitHub/CameraTrapStudy/2015 data/pics.wide20160804.RData")
  load("C:/Users/anna.moeller/Documents/GitHub/CameraTrapStudy/2015 data/pics.wide20160804.RData")
  
###############################################
  # Add an event ID to every sighting of an elk
  source("GitHub/CameraTrapStudy/Image Analysis/eventID_fn.R")
  
  # Make an elk dataframe that is smaller to work with
  elkdata <- select(pics, site, plot, cam, timeLST, dateLST, opstate, trigger, viewer, 
                    DateProcessed, uniquemark, comment, SourceFile, 
                    grep("elk", names(pics), ignore.case = T)) %>%
    do(eventID_fn(data = ., species = "elk", cutoff = 30))
  
  # Check that each eventID has some count in it
  tot <- group_by(elkdata, eventID) %>%
    summarise(site = min(site),
              cam = min(cam),
              eventstart = min(timeLST),
              cow = sum(ElkAntlerless), 
              calf = sum(ElkCalf),
              spike = sum(ElkSpike),
              rag = sum(ElkRaghorn),
              mat = sum(ElkMatBull),
              peds = sum(ElkNubsPeds),
              unkn = sum(ElkUnkn)) %>%
    mutate(tot = cow + calf + spike + rag + mat + peds + unkn)
  
  # Look at the events with a count of 0
  length(which(tot$tot == 0))
  
  # For thesis, how long are events?
  tst <- elkdata %>%
    filter(eventID > 0) %>%
    group_by(eventID)  %>%
    summarise(len = length(eventID),
              min = min(as.numeric(timeLST)),
              max = max(as.numeric(timeLST))) %>%
    mutate(t = max - min)
  summary(tst$t)
  # Mean event = 1282 seconds = 21 minutes
  
  summary(tst$len)
  tst2 <- tst %>%
    filter(len > 1000)
  elkdata %>%
    filter(eventID %in% tst2$eventID) %>%
    group_by(eventID) %>%
    summarise(sf = first(SourceFile))

###############################################
  # Make an elk encounter history
  source("C:/Users/anna.moeller/Documents/GitHub/CameraTrapStudy/Image Analysis/eh_fn.R")
  elk.eh <- eh_fn(pics, access, starthour = "00:00:00", endhour = "23:00:00", 
                  by_t = "hour", animal.eh = T)

#################################################
  # Create an effort encounter history for all the cameras
  source("C:/Users/anna.moeller/Documents/GitHub/CameraTrapStudy/Image Analysis/eh_fn.R")
  cam.eh <- eh_fn(pics, access, starthour = "12:00:00", endhour = "12:00:00", 
                  by_t = "hour", animal.eh = F)
  
  # For looking at the days cameras didn't function
  # Cameras we know don't have full pictures are:
  notfull <- c("AM112", "AM128", "AM132", "AM133", "AM140", "AM151", "AM152", "AM155", 
               "AM157", "AM169", "AM176", "AM184", "AM202", "AM203", "AM204", "AM30", "AM33", 
               "AM34", "AM57", "AM62", "AM69", "AM76", "AM78", "AM79", "AM82", "AM87", "AM89", 
               "AM888", "AM999", "AM101", "AM104", "AM201", "AM118", "AM56", "AM92")
  
  table(eh$cam[eh$open == F & !(eh$cam %in% notfull)])
  unique(as.Date(eh$ideal)[eh$cam == "AM101" & eh$open == F])
  head(eh[eh$cam == "AM100" & eh$open == F,])
  
########################################################
  # Checks on the data

  # Find any missing opstates
  pics$SourceFile[(!is.na(pics$opstate) & pics$opstate == "") | 
                     (!is.na(pics$opstate) & pics$opstate == " ")]
  
  # Check that viewer is filled out on all the photos that have been looked at
  any(is.na(pics$viewer))
  
  # Check for malfunction as the first operation state
  data <- group_by(pics, cam) %>%
          summarise(opstate = first(opstate))
  table(data$opstate)
  # Yeah, it exists but I'm not going to worry about it
  
  # Check for weird count values
  # species: elk, md, wtd, prong, human, predator, other
  source("GitHub/CameraTrapStudy/2015 data/tolong.fn.R")
  elk <- tolong.fn(pics, species = "elk")
  table(elk$count)
  # All reasonable for elk
  
  # Look at the eventIDs where the count is 0
  # See count0_exper

  # When I bind_rows the records, it fills in NAs for the "other" species, 
  #  which should be 0s. Fix this if I ever actually care
  
  # Check whether "Elk present" is checked whenever an elk is counted
  any(elk$elkpresent[!is.na(elk$count) & elk$count != 0] == F)
  tst <- elk[!is.na(elk$count) & elk$count > 0, ]
  tst[which(tst$elkpresent == F), ]
  # I fixed some of them, but they were only off by a tiny bit. It's probably not worth worrying
  #   about the rest of them

  # Check out all the "review" pictures # DONE
  pics[pics$review == T, ]
  
  # Photos:
  # Do I have 215 folders of photos to get SD cards? (19 + 43 + 80 + 80 - 6 stolen - 1 lost)
  length(unique(out$cam_tr)) # Nope. I have 213 (which translates to 216 because 3 are in the same folder)
  
  # Access: 
  # Do I have 222 entries in my Important Info?
  length(access$camID) # Yes
  length(unique(access$camID)) # 162 unique cameras
  # I checked in excel that the site name in Deployment == site name in Battery Check
  # Check that the Deployment ID is the same as the cam name
  #tmp <- gsub("(AM.*)_15[ab]", "\\1", access$DeploymentID)
  #any(tmp != access$camID) # Great. 
  # Does the number of photos in the folder match the number stated in Access? 
  
  # Separate out the photos from the two deployments at AM33, AM40

  
  
###########################################################
  # Pull out great pics
  from <- pics$SourceFile[!is.na(pics$greatpic) & pics$greatpic == T]
  fromF<- gsub("^H:", "F:", from)
  file.copy(from = from, to = "C:/Users/anna.moeller/Pictures/Greatpics")
  
  # And collars
  from <- pics$SourceFile[!is.na(pics$uniquemark) & pics$uniquemark == T]
  file.copy(from = from, to = "C:/Users/anna.moeller/Pictures/Greatpics/Collars")
  
  # And pictures with funny comments
  funny <- unique(pics$comment)[c(8,12,28:31,33,35:38,42,43,60:65,70,78,81,82,104,108:111, 124,125,128:131,
                         139,144,146,155,156,159)]
  from <- pics$SourceFile[pics$comment %in% funny]
  from <- gsub("^F:", "H:", from)
  file.copy(from = from, to = "C:/Users/anna.moeller/Pictures/Greatpics/Funny comments")
#############################################################################
  # Stuff I don't need anymore 
    # 
    # # 6. Make a checklist of all the files that need to be analyzed 
    # # Find out which files have or don't have Timelapse files
    # files <- lapply(trips, list.files, pattern = "TimelapseData.csv")
    # notimelapse <- trips[which(lapply(files, length) == 0)]
    # timedone <- trips[which(lapply(files, length) != 0)]
    # #load("C:/Users/anna.moeller/Documents/GitHub/CameraTrapStudy/2015 data/timedone.RData")
    # 
    # # Create all the columns in that checklist
    # xx <- gsub(".*/2015-16/(.*)", "\\1", trips)
    # site <- gsub("(.*)/AM.*", "\\1", xx)
    # cam_tr <- gsub(".*(AM.*/Trip.*)/10.*", "\\1", xx)
    # cam <- gsub("(AM.*)/Trip.*", "\\1", cam_tr)
    # col2 <- gsub(".*(AM.*/Trip.*)", "\\1", xx)
    # 
    # # Check off which ones have been done
    # done <- gsub(".*/(AM.*)", "\\1", timedone)
    # col3 <- ifelse(col2 %in% done, "MM", "")
    # 
    # # Write it 
    # out <- data.frame("site" = site, "folder" = col2, "done" = col3, "cam_tr" = cam_tr, "cam" = cam)
    # #    prettier <- data.frame("Site" = site, Folder = col2, Done = col3)
    # #    write.csv(prettier, file = "C:/Users/anna.moeller/Documents/Camera Trap Study/Photos/Photos Analyzed.csv")
    # 
    # 
  # cols <- c("ElkSpike", "ElkRaghorn", "ElkMatBull", "Elknubspeds", "ElkAntlerless", 
  #           "ElkCalf", "ElkUnkn", "MDbuck", "MDantlerless", "MDfawn", "MDunkn", 
  #           "WTDbuck", "WTDantlerless", "WTDfawn", "WTDunkn", "MooseBull", 
  #           "MooseAntlerless", "MooseCalf", "MooseUnkn", "PronghornBuck", "PHdoe",
  #           "PronghornFawn", "PHunkn", "BlackBearAdult", "BlackBearCub", "LionAdult", 
  #           "LionKitten", "WolfAdult", "WolfPup",  "CattleCow", "CattleCalf",
  #           oth.cols)
