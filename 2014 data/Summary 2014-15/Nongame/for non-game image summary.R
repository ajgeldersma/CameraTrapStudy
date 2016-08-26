# Non-game summary
# Anna Moeller
# 11/19/2015

# Load packages
library(dplyr)

# Open s.fn and ig.fn (import 2014)
source("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Image Analysis/import 2014.R")
swin <- s.fn()
bh <- bh.fn()

# Make columns the same
swin2 <- swin
swin2$present[swin$elk.present == ""] <- swin$animals.present[swin$elk.present == ""]
swin2$present[swin$animals.present == ""] <- swin$elk.present[swin$animals.present == ""]
swin2 <- mutate(swin2,
                MarkForDeletion = NA,
                opstate = op.state,
                collaryn = collar.yn,
                animalother = animal.other,
                animalunkn = animal.unkn,
                ImageQuality = Image.Quality) %>%
  select(-op.state, -collar.yn, -Image.Quality, -animals.present, 
         -elk.present, -uncounted, -animal.other, -animal.unkn)
  
# Stick 'em together
tmp <- bind_rows(swin2, bh) 

# Change back the 999 value
tmp$ph[which(tmp$ph == 999)] <- 20

# Summary
nong <- tmp %>%
  group_by(cam) %>%
  summarise(elk = sum(elkcow, elkcalf, elkbull, elkunkn, na.rm = T), 
            muledeer = sum(md),
            pronghorn = sum(ph),
            whitetaildeer = sum(wtd),
            coyote = sum(coyote),
            lion = sum(lion),
            turkey = sum(turkey),
            lagomorph = sum(lagomorph))

# Write it to an excel file
write.csv(nong, file = "C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Image Analysis/nongame_summary.csv")
