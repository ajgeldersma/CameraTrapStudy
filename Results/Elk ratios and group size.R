  # Look at elk ratios 
  # Anna Moeller
  # 8/4/2016
  
  # Load packages
  source("GitHub/packages.R")
  
  # Bring in databases
  load("GitHub/CameraTrapStudy/2015 data/pics.wide20160804.RData")
  
  # Add an event ID to every sighting of an elk
  source("GitHub/CameraTrapStudy/Image Analysis/eventID_fn.R")
  
  # Make an elk dataframe that is smaller to work with
  elkdata <- select(pics, site, plot, cam, timeLST, dateLST, opstate, trigger, viewer, 
                    DateProcessed, uniquemark, comment, SourceFile, 
                    grep("elk", names(pics), ignore.case = T)) %>%
    # Add eventID
    do(eventID_fn(., species = "elk", cutoff = 120)) %>%
    mutate(bulls = ElkSpike + ElkRaghorn + ElkMatBull + ElkNubsPeds,
           total = bulls + ElkAntlerless + ElkCalf + ElkUnkn) %>%
    
    # Take out eventID == 0 and total == 0
    filter(eventID != 0, 
           total > 0)

########################################################
  # Age and Sex
  
  # Proportions of each age/sex category by month
  prop <- elkdata %>%
    mutate(month = month(dateLST)) %>%
    group_by(site, month) %>%
    summarise_each(funs(sum), c(14:20, 22:23)) %>%
    mutate(p.antlerless = round(ElkAntlerless/total, 3),
           p.calf = round(ElkCalf/total, 3),
           p.bulls = round(bulls/total, 3),
           p.unkn = round(ElkUnkn/total, 3)) %>%
    select(site, month, total, ElkAntlerless, ElkCalf, bulls, ElkUnkn, p.antlerless, p.calf, p.bulls, p.unkn)
  # These look pretty weird. 
  
  # What if we look at group composition for groups > 1
  avggrp <- elkdata %>%
    group_by(site, eventID) %>%
    summarise_each(funs(sum), c(14:20, 22:23)) %>%
    filter(total > 1) %>%
    mutate(p.antlerless = round(ElkAntlerless/total, 3),
           p.calf = round(ElkCalf/total, 3),
           p.bulls = round(bulls/total, 3),
           p.unkn = round(ElkUnkn/total, 3))  %>%
    ungroup() %>%
    group_by(site) %>%
    summarise_each(funs(round(mean(.), 3)), 11:15)
    # Didn't really change much
  
####################################################
  # Timing of elk detections
  
  # Look at the timing of the number of groups
  tmp <- elkdata %>%
    mutate(month = month(dateLST)) %>%
    group_by(site, eventID) %>%
    summarise(total = sum(total),
              month = min(month))
  
  # Timing of groups of different sizes
  singles <- tmp %>% filter(total == 1)
  bigger <- tmp %>% filter(total > 1)

  par(mfrow = c(1,1))
  barplot(prop.table(table(tmp$month)), main = "Number of Groups", xlab = "month")
  barplot(prop.table(table(singles$month)), main = "Number of Singleton Groups", xlab = "month")
  barplot(prop.table(table(bigger$month)), main = "Number of Groups > 1", xlab = "month")
  
  par(mfrow = c(2,1))
  barplot(prop.table(table(tmp$month[tmp$site == "Beaverhead"])), main = "Beaverhead Number of Groups", xlab = "month")
  barplot(prop.table(table(tmp$month[tmp$site == "St. Joe"])), main = "St. Joe Number of Groups", xlab = "month")
  
  barplot(prop.table(table(singles$month[singles$site == "Beaverhead"])), main = "Beaverhead Number of Singletons", xlab = "month")
  barplot(prop.table(table(singles$month[singles$site == "St. Joe"])), main = "St. Joe Number of Singletons", xlab = "month")
  
  barplot(prop.table(table(bigger$month[bigger$site == "Beaverhead"])), main = "Beaverhead Number of Groups > 1", xlab = "month")
  barplot(prop.table(table(bigger$month[bigger$site == "St. Joe"])), main = "St. Joe Number of Groups > 1", xlab = "month")
  
##################################################
  # Group size
  
  # Plot group size for Beaverhead Feb 1-Feb 13 for manuscript
  datelim <- as.Date(c("2016-02-01", "2016-02-13"))
  feb <- elkdata %>%
    filter(eventID != 0,
           total != 0,
           site == "Beaverhead",
           datelim[1] <= dateLST & dateLST <= datelim[2]) %>%
    group_by(eventID) %>%
    summarise(site = min(site),
              grpsize = sum(total),
              length = max(timeLST) - min(timeLST))
  hist(feb$grpsize, main = NULL, xlab = NULL, breaks = 10)
  # saved it

  # Group size for every elk event
  comp <- elkdata %>%
    filter(eventID != 0,
           total != 0) %>%
    group_by(eventID) %>%
    summarise(site = min(site),
              grpsize = sum(total),
              length = max(timeLST) - min(timeLST))
  
  # Summary of group size
  ungroup(comp) %>%
    group_by(site) %>%
    summarise(mean = mean(grpsize),
              min = min(grpsize),
              max = max(grpsize),
              sd = sd(grpsize))
  
  # Do it for just Feb
  # Group size for every elk event
  feb <- elkdata %>%
    filter(eventID != 0,
           total != 0,
           month(dateLST) == 2) %>%
    group_by(eventID) %>%
    summarise(site = min(site),
              grpsize = sum(total),
              length = max(timeLST) - min(timeLST))
  # Plots
  par(mfrow = c(1,2),
      mar = c(5,4,4,2),
      oma = c(1, 0, 0, 0))
  bb <- feb %>% 
    filter(site == "Beaverhead") 
  hist(bb$grpsize, main = "Beaverhead", xlab = NULL)
  
  pp <- feb %>% 
    filter(site == "St. Joe") 
  hist(pp$grpsize, main = "Panhandle", xlab = NULL, ylab = NULL)
  
  mtext("Group Size", side = 1, outer = T, line = -2)
  
  # Most events are coming from a single picture
  # barplot(prop.table(table(comp$length[comp$site == "Beaverhead"])), main = "Beaverhead Event Length", xlab = "Minutes")
  # barplot(prop.table(table(comp$length[comp$site == "St. Joe"])), main = "St. Joe Event Length", xlab = "Minutes")
  hist(as.numeric(comp$length[comp$site == "Beaverhead"]), main = "Beaverhead Event Length", xlab = "Minutes", breaks = 100)
  hist(as.numeric(comp$length[comp$site == "St. Joe"]), main = "St. Joe Event Length", xlab = "Minutes", breaks = 100)
  
  # Plot group size
  par(mfrow = c(1,2))
  barplot(prop.table(table(comp$grpsize[comp$site == "Beaverhead"])), main = "Beaverhead Group Size", xlab = "Elk in one event")
  barplot(prop.table(table(comp$grpsize[comp$site == "St. Joe"])), main = "St. Joe Group Size", xlab = "Elk in one event")

  # What if I only looked at events from more than a single picture? 
  tst <- comp %>%
    filter(length > 0) 
  barplot(prop.table(table(tst$grpsize[tst$site == "Beaverhead"])), main = "Beaverhead", xlab = "Group Size without Single Pictures")
  barplot(prop.table(table(tst$grpsize[tst$site == "St. Joe"])), main = "St. Joe", xlab = "Group Size without Single Pictures")
  
  # Look at randomly selected elk events to see if most really are a single animal 
  set.seed(212)
  samp <- sample(1:1240, 15)
  xx <- elkdata %>%
    filter(eventID %in% samp) %>%
    group_by(eventID) %>%
    summarise(SourceFile = min(SourceFile),
              length = max(timeLST) - min(timeLST),
              grpsize = sum(total))