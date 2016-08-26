# Finding human pictures in Panhandle
# Anna Moeller  
# 6/26/2015

# Set working directory
setwd("C:/Users/anna.moeller/Pictures/RECONYX Images/Panhandle")

# Open all the folders in Panhandle and find the Image Data from Timelapse
folders <- grep("AM", x = list.files(), value = T)
m <- sapply(folders, list.files, pattern = "ImageData.csv", full.names = T)

# Can't bind them because they're in different formats

# Open them up individually, I guess.
data <- read.csv(m[18], as.is = T)
head(data)

# Check for humans
any(data$animal.other > 0)
any(is.na(data$comments))
any(data$human > 0)

# Starting with AM28, "no animals present"
unique(data$elk.present)

data[which(data$human > 0), ]

which(!is.na(data$comments))
data[which(data$animal.other > 0),]


