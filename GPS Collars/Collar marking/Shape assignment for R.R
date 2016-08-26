# Adding shape columns
# Anna Moeller
# 6/10/2015

# Load packages
library(dplyr)

# load data
tmp <- read.csv("C:/Users/anna.moeller/Documents/Camera Trap Study/Marking collars/shape assignment export.txt", 
                   header = F)

# Add column names
colnames(tmp) <- c("study area", "species", "key", "shapeID", "shapeID2", 
                      "brand", "serial", "animalID", "age", "cattle tag", "notes")

# Create three columns with words for the symbols
shapes <- mutate(tmp,
                 collar1 = substr(shapeID, 1, 1), 
                 collar2 = substr(shapeID, 2, 2), 
                 collar3 = substr(shapeID, 3, 3))

shapes$collar1[which(shapes$collar1 == 1)] <- "square"
shapes$collar1[which(shapes$collar1 == 2)] <- "circle"
shapes$collar1[which(shapes$collar1 == 3)] <- "triangle"
shapes$collar1[which(shapes$collar1 == 4)] <- "heart"
shapes$collar1[which(shapes$collar1 == 5)] <- "star"
shapes$collar1[which(shapes$collar1 == 6)] <- "moon"
shapes$collar1[which(shapes$collar1 == 7)] <- "S"
shapes$collar1[which(shapes$collar1 == 8)] <- "X"

shapes$collar2[which(shapes$collar2 == 1)] <- "square"
shapes$collar2[which(shapes$collar2 == 2)] <- "circle"
shapes$collar2[which(shapes$collar2 == 3)] <- "triangle"
shapes$collar2[which(shapes$collar2 == 4)] <- "heart"
shapes$collar2[which(shapes$collar2 == 5)] <- "star"
shapes$collar2[which(shapes$collar2 == 6)] <- "moon"
shapes$collar2[which(shapes$collar2 == 7)] <- "S"
shapes$collar2[which(shapes$collar2 == 8)] <- "X"

shapes$collar3[which(shapes$collar3 == 1)] <- "square"
shapes$collar3[which(shapes$collar3 == 2)] <- "circle"
shapes$collar3[which(shapes$collar3 == 3)] <- "triangle"
shapes$collar3[which(shapes$collar3 == 4)] <- "heart"
shapes$collar3[which(shapes$collar3 == 5)] <- "star"
shapes$collar3[which(shapes$collar3 == 6)] <- "moon"
shapes$collar3[which(shapes$collar3 == 7)] <- "S"
shapes$collar3[which(shapes$collar3 == 8)] <- "X"

shapes <- mutate(shapes,
                 design = paste(collar1, collar2, collar3))

write.csv(shapes, file = "C:/Users/anna.moeller/Documents/Camera Trap Study/Marking collars/shape assignment for R.csv")

