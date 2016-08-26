# Import and format all the files from winter 2014-15
# Anna Moeller
# 3/19/2016

# I used 8 different formats. ICK! (Notes in thesis notes notebook)
# 1: S AM11, S AM12, S AM13, S AM14, S AM16, IG AM21, IG AM22
# 2: S AM17, S AM18, S AM19, S AM20
# 3. IG AM23, IG AM28, IG AM33, IG AM38, IG AM39, IG AM40, IG AM47, DC AM41
# 4. DC AM42, DC AM43
# 5. DC AM45
# 6. DC AM49, DC AM51, DC AM53, DC AM57, BS AM46, BS AM48, BS AM52, BS AM54, BS AM55, BS AM56
# 7. BS AM58, BS AM59
# 8. BS AM61, all of Buckhorn, all of Kenney Creek


# Load packages
library(dplyr)
library(lubridate)

# Set working directory
setwd("C:/Users/anna.moeller/Pictures/RECONYX Images/2014-15")

import2014.fn <- function(cam, ver){
  # Take a camera, find its region
  if(grepl("BH|KC", cam)){
    region <- "Beaverhead Reg. 7"
    site <- substr(cam, 1, 2)
  } else if (grepl("DC|BS", cam)) {
    region <- "Beaverhead Reg. 6"
    site <- substr(cam, 1, 2)
  } else if (grepl("^S", cam)) {
    region <- "Panhandle"
    site <- substr(cam, 1, 1)
  } else if (grepl("IG", cam)) {
    region <- "Panhandle"
    site <- substr(cam, 1, 2)
  }
  
  datafile <- list.files(paste(region, cam, sep = "/"), pattern = "Data.csv", full.names = T)
  
  # If it is timelapse version 2, call v2, else call v1 
  source("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/2014 data/combine metadata and timelapse.R")
  if(grepl("TimelapseData.csv", datafile)){
    out <- combo_timelapse2_fn(region = region, site = site, cam = cam)
  } else if (grepl("ImageData.csv", datafile)){
    out <- combo_timelapse1_fn(region = region, site = site, cam = cam)
  }
  
  stopifnot(dim(out)[1] > 0)
  
  # Earlier versions will add: 
  if(ver %in% 1:7){
    out <- mutate(out, 
                  review = NA)
  }
  
  if(ver %in% 3:6){
    out <- rename_(out, 
                   present = as.name(names(out)[grep("present", names(out))])) %>%
      rename(ImageQuality = Image.Quality,
             collaryn = collar.yn,
             animalother = animal.other,
             animalunkn = animal.unkn,
             opstate = op.state) %>%
      mutate(MarkForDeletion = NA,
             opstate = tolower(opstate),
             present = tolower(present),
             present = replace(present, opstate %in% c("setup", "takedown"), "humans present"),
             bird = NA)
  }

  if(ver %in% 1:2){
    out <- rename(out, 
                  ImageQuality = Image.Quality,
                  collaryn = collar.yn,
                  opstate = op.state,
                  animalother = animal.other,
                  animalunkn = animal.unkn) %>%
      mutate(MarkForDeletion = NA,
             opstate = tolower(opstate),
             present = "no animals present",
             present = replace(present, grepl("elk", elk.present, ignore.case = T) & !(grepl("animal", animals.present, ignore.case = T)), "elk present"),
             present = replace(present, !(grepl("elk", elk.present, ignore.case = T)) & grepl("animal", animals.present, ignore.case = T), "other animals present"),
             present = replace(present, grepl("elk", elk.present, ignore.case = T) & grepl("animal", animals.present, ignore.case = T), "elk and other animals present"),
             present = replace(present, opstate %in% c("setup", "takedown"), "humans present"),
             cattle1 = ifelse(cattle.tag == "can't see", "unknown", substr(cattle.tag, 1, 1)),
             cattle1 = ifelse(grepl("\\?", cattle1), "unknown", cattle1),
             cattle2 = ifelse(cattle.tag == "can't see", "unknown", substr(cattle.tag, 2, 2)),
             cattle2 = ifelse(grepl("\\?", cattle2), "unknown", cattle2),
             bird = NA) %>%
      select(-elk.present, -animals.present, -cattle.tag)
  }
  
  # Because this camera didn't have the option of "tilted"
  if(cam == "BS AM46"){
    out <- mutate(out, 
                  opstate = replace(opstate, opstate == "Tampered with", "tilted"))
  }
  
  if(ver %in% 1:5){
    out <- mutate(out,
                  sheep = NA,
                  cattle = NA,
                  horse = NA,
                  opstate = replace(opstate, opstate %in% c("", " "), "operating"))
  }
  
  if(ver %in% 1:4){
    out <- mutate(out, 
                  background = NA,
                  uncounted = tolower(uncounted),
                  uncounted = replace(uncounted, !is.na(uncounted) & (uncounted == "" | uncounted == " "), NA))
  }
  
  # Later versions may have 999. Replace these.
  if(ver %in% 5:8){
    out <- mutate(out,
                  uncounted = NA,
                  uncounted = replace(uncounted, !is.na(bird) & bird == 999, "bird"),
                  uncounted = replace(uncounted, !is.na(sheep) & sheep == 999, "sheep"),
                  uncounted = replace(uncounted, !is.na(ph) & ph == 999, "ph"),
                  uncounted = replace(uncounted, !is.na(animalunkn) & animalunkn == 999, "animalunkn"),
                  uncounted = replace(uncounted, !is.na(animalother) & animalother == 999, "animalother"),
                  bird = replace(bird, !is.na(bird) & bird == 999, 0),
                  sheep = replace(sheep, !is.na(sheep) & sheep == 999, 0),
                  ph = replace(ph, !is.na(ph) & ph == 999, 0),
                  animalunkn = replace(animalunkn, !is.na(animalunkn) & animalunkn == 999, 0),
                  animalother = replace(animalother, !is.na(animalother) & animalother == 999, 0),
                  background = tolower(background),
                  background = replace(background, !is.na(background) & (background == "" | background == " "), "none"))
   }
  
  # Some blanks were imported as NAs. Now after all that work, lets change all NAs to 0s
  out <- mutate(out, 
                elkcalf = replace(elkcalf, is.na(elkcalf), 0),
                elkcow = replace(elkcow, is.na(elkcow), 0),
                elkbull = replace(elkbull, is.na(elkbull), 0),
                elkunkn = replace(elkunkn, is.na(elkunkn), 0),
                md = replace(md, is.na(md), 0),
                ph = replace(ph, is.na(ph), 0),
                wtd = replace(wtd, is.na(wtd), 0),
                coyote = replace(coyote, is.na(coyote), 0),
                fox = replace(fox, is.na(fox), 0),
                moose = replace(moose, is.na(moose), 0),
                lion = replace(lion, is.na(lion), 0),
                wolf = replace(wolf, is.na(wolf), 0),
                turkey = replace(turkey, is.na(turkey), 0),
                sg = replace(sg, is.na(sg), 0),
                bird = replace(bird, is.na(bird), 0),
                lagomorph = replace(lagomorph, is.na(lagomorph), 0),
                human = replace(human, is.na(human), 0),
                sheep = replace(sheep, is.na(sheep), 0),
                cattle = replace(cattle, is.na(cattle), 0),
                horse = replace(horse, is.na(horse), 0),
                bobcat = replace(bobcat, is.na(bobcat), 0),
                marten = replace(marten, is.na(marten), 0),
                fisher = replace(fisher, is.na(fisher), 0),
                lynx = replace(lynx, is.na(lynx), 0),
                animalother = replace(animalother, is.na(animalother), 0),
                animalunkn = replace(animalunkn, is.na(animalunkn), 0))
  return(out)
}

# Import version 8:
reg7 <- list.files("Beaverhead Reg. 7", pattern = "AM")
cam <- c("BS AM61", reg7)
out <- do.call(rbind, lapply(cam, import2014.fn, ver = 8))

# Import version 7:
cam <- c("BS AM58", "BS AM59")
out2 <- do.call(rbind, lapply(cam, import2014.fn, ver = 7))
out <- rbind(out, out2)

# Import version 6:
cam <- c("DC AM49", "DC AM51", "DC AM53", "DC AM57", "BS AM46", "BS AM48", "BS AM52", "BS AM54", "BS AM55", "BS AM56")
out2 <- do.call(rbind, lapply(cam, import2014.fn, ver = 6))
out <- rbind(out, out2)

# Import version 5:
cam <- "DC AM45"
out2 <- do.call(rbind, lapply(cam, import2014.fn, ver = 5))
out <- rbind(out, out2)

# Import version 4:
cam <- c("DC AM42", "DC AM43")
out2 <- do.call(rbind, lapply(cam, import2014.fn, ver = 4))
out <- rbind(out, out2)

# Import version 3:
cam <- c("IG AM23", "IG AM28", "IG AM33", "IG AM38", "IG AM39", "IG AM40", "IG AM47", "DC AM41")
out2 <- do.call(rbind, lapply(cam, import2014.fn, ver = 3))
out <- rbind(out, out2)

# Import version 2: 
cam <- c("S AM17", "S AM18", "S AM19", "S AM20")
out2 <- do.call(rbind, lapply(cam, import2014.fn, ver = 2))
out <- rbind(out, out2)

# Import version 1:
cam <- c("S AM11", "S AM12", "S AM13", "S AM14", "S AM16", "IG AM21", "IG AM22")
out2 <- do.call(rbind, lapply(cam, import2014.fn, ver = 1))
out <- rbind(out, out2)

# This finally looks good! Let's save it!
#write.csv(out, "C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/2014 data/2014data.csv")
#save(out, file = "C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/2014 data/2014data.RData")


# names(out)[!(names(out) %in% names(out2))]
# names(out2)[!(names(out2) %in% names(out))]

# # Make sure none of the animal counts are 999
# check <- apply(out, 2, function(x){
#   any(x == 999)
# })
# which(check)
# out[!is.na(out$animalother) & out$animalother == 999, ]

# Replace any NAs (from blanks) in the count columns with 0s
# Blanks in the counts should be changed to 0s
# countcols <- c("elkcalf", "elkcow", "elkbull", "elkunkn", "md", "ph", "wtd", "coyote",
#                "fox", "moose", "lion", "wolf", "turkey", "sg", "bird", "lagomorph", "human",
#                "sheep", "cattle", "horse", "fox", "bobcat", "marten", "fisher", "lynx", 
#                "animalother", "animalunkn")
# check <- apply(out[, names(out) %in% countcols], 2, function(x){
#   any(is.na(x))
# })

# Whoa. huge thing to remember: mutate_each and replace
# replace only works on character and numeric strings

# Things to check
# table(out$uncounted)
# table(out$background)
# table(out$present)
# table(out$opstate)
# length(unique(out$cam))
