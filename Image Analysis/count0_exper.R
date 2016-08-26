  # Count = 0 Experiment 
  # Anna Moeller
  # 8/3/2016
  
  # potential sources of eventID = 0
  # 1. Elk present accidentally checked on empty pictures
  # 2. Some other "present" (or none) accidentally checked when Elk are counted
  # 3. Elk are accidentally misidentified as other animals (wrong click or confusion)
  # 4. Elk present checked but viewer forgot to count individuals
  # 5. Elk bedded and there is a true gap
  
  # Checks
  # 1. Check whether at least one animal is recorded when "elk present" is checked (not complete check)

  # 2. Check whether "Elk present" is checked whenever an elk is counted
  any(elk$elkpresent[!is.na(elk$count) & elk$count != 0] == F)
  tst <- elk[!is.na(elk$count) & elk$count > 0, ]
  tst[which(tst$elkpresent == F), ]
  # I fixed some of them, but they were only off by a tiny bit. It's probably not worth worrying
  #   about the rest of them
  
  # 3. Check for "elk present" and no other "present", but with a count in elk column
  #   this also counts incorrect "elk present" when it's really a different animal
  
  # 4. Look at photos
  
  # 5. Check whether at least one animal is recorded when "elk present" is checked (not complete check)
  
############################
  # Take a look at the eventID == 0
  source("GitHub/Camera-trap-study/Image Analysis/eventID_fn.R")
  
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
  
  # Which eventIDs are these?
  ev0 <- filter(tot, tot == 0) %>%
    select(eventID, site, cam, eventstart)
  
  # Two of the cameras account for most of these...  bedding?
  table(ev0$cam)
  
  # Sample 20% of these. n = 31
  length(ev0$eventID) * .2
  set.seed(21)
  samp <- sample(ev0$eventID, 31)
  totest <- filter(ev0, eventID %in% samp)
  
  