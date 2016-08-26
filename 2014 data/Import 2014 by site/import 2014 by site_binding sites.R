# Save 2014-15 image data as .csvs
# Anna Moeller
# 3/18/2016

# Load packages
library(dplyr)
library(lubridate) # Not totally sure I need this one

# Set working directory 
setwd("C:/Users/anna.moeller/Pictures/RECONYX Images/2014-15")

# Source the import functions
source("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Image Analysis/Import image data/import 2014.R")

# Call all the functions
bs <- bs.fn()
dc <- dc.fn()
ig <- ig.fn()
s <- s.fn()
bh <- bh.fn()
kc <- kc.fn()

# Make them all the same structure
bs <- mutate(bs,
             ImageQuality = Image.Quality,
             opstate = op.state,
             animalother = animal.other,
             animalunkn = animal.unkn,
             collaryn = collar.yn,
             MarkForDeletion = NA,
             site = "BS") %>%
  select(-Image.Quality, -op.state, -animal.other, -animal.unkn, -collar.yn)
bs$uncounted <- NA
bs$uncounted[bs$animalunkn == 999] <- "animalunkn"
bs$animalunkn[bs$animalunkn == 999] <- 0

dc <- mutate(dc,
             ImageQuality = Image.Quality,
             opstate = op.state,
             animalother = animal.other,
             animalunkn = animal.unkn,
             collaryn = collar.yn,
             MarkForDeletion = NA,
             review = NA,
             site = "DC") %>%
  select(-Image.Quality, -op.state, -animal.other, -animal.unkn, -collar.yn)
dc$uncounted <- NA
dc$uncounted[dc$animalother == 999] <- "animalother"
dc$uncounted[dc$sheep == 999] <- "sheep"
dc$animalother[dc$animalother == 999] <- 0
dc$sheep[dc$sheep == 999] <- 0

ig <- mutate(ig,
             ImageQuality = Image.Quality,
             opstate = tolower(op.state),
             animalother = animal.other,
             animalunkn = animal.unkn,
             collaryn = collar.yn,
             MarkForDeletion = NA,
             review = NA,
             sheep = NA,
             cattle = NA,
             horse = NA,
             background = NA,
             site = "IG") %>%
  select(-Image.Quality, -op.state, -animal.other, -animal.unkn, -collar.yn, -cattle.tag)

s <- mutate(s,
            ImageQuality = Image.Quality,
            opstate = tolower(op.state),
            animalother = animal.other,
            animalunkn = animal.unkn,
            collaryn = collar.yn,
            MarkForDeletion = NA,
            review = NA,
            sheep = NA,
            cattle = NA,
            horse = NA,
            background = NA,
            site = "S") %>%
  select(-Image.Quality, -op.state, -animal.other, -animal.unkn, -collar.yn)

bh <- mutate(bh, 
             site = "BH",
             uncounted = NA)
bh$uncounted[bh$ph == 999] <- "ph"
bh$ph[bh$ph == 999] <- 0

kc <- mutate(kc, 
             site = "KC", 
             uncounted = NA)
kc$uncounted[kc$bird == 999] <- "bird"
kc$bird[kc$bird == 999] <- 0

# Okay. Now stick them all together
data <- bind_rows(bs, dc, ig, s, bh, kc)

# Save them all as .csvs
write.csv(bs, "C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/2014 data/Boulder Spring.csv")
write.csv(dc, "C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/2014 data/Deer Canyon.csv")
write.csv(ig, "C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/2014 data/Italian Gulch.csv")
write.csv(s, "C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/2014 data/Swinnerton.csv")
write.csv(bh, "C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/2014 data/Buckhorn.csv")
write.csv(kc, "C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/2014 data/Kenney Creek.csv")
write.csv(data, "C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/2014 data/data 2014.csv")


################################################ Checks ##############
# 
# # Make sure none of the animal counts are 999
# check <- apply(data, 2, function(x){
#   any(x == 999)
# })
# which(check)
# 
# # uncounted dc, ig, s
# dim(bs)
# dim(dc)
# dim(ig)
# dim(s)
# dim(bh)
# dim(kc)

# Why is this longer than the number of pictures I calculated? 
dim(data)
