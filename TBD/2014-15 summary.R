# Number of photos in 2014-15
# Anna Moeller
# 9/3/2015

# Read in data from Access
x <- read.csv("C:/Users/anna.moeller/Documents/Camera Trap Study/Cameras/Deployment.csv")

# Prettify it
names(x) <- c("ID", "camID", "batt", "sd", "cable", "key", "loc", "deploy", 
              "redeploy", "batt_life", "pics", "takedown", "censor", "notes")
x <- mutate(x,
           deploy = as.Date(as.character(deploy), format = "%m/%d/%Y"),
           redeploy = as.Date(as.character(redeploy), format = "%m/%d/%Y"),
           takedown = as.Date(as.character(takedown), format = "%m/%d/%Y"),
           censor = as.Date(as.character(censor), format = "%m/%d/%Y"),
           batt_life = as.numeric(sub("%", "", batt_life))/100)

x$zone <- ifelse(x$loc == "Swinnerton" | x$loc == "Italian Gulch", "Panhandle", "Beaverhead")
x$region <- ifelse(x$loc == "Kenney Creek" | x$loc == "Buckhorn", "Region 7", "Region 6")
x$region[x$zone == "Panhandle"] <- "Region 1"

# Summarise the number of photos taken at each camera
x2 <- group_by(x, camID, zone) %>%
  summarise(pics = sum(pics))

# Average and range number of photos taken in Panhandle
x3 <- group_by(x2, zone) %>%
  summarise(avg = mean(pics, na.rm = T),
            min = min(pics, na.rm = T),
            max = max(pics, na.rm = T))

# Total number of pictures
sum(x2$pics, na.rm = T)

# Camera days
# if there is a censor date, take that over takedown
# if there are 2 records for a camID, take the min deploy and max takedown

x4 <- data.frame(camID = rep(NA, times = length(unique(x$camID))))

x4 <- NULL
x4$camID <- unique(x$camID)
x4$deploy <- 
  
m <- 
  
#################################################################################

# Read in image analysis data
source("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Image Analysis/import Boulder Spring.R")
source("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Image Analysis/import Deer Canyon.R")
source("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Image Analysis/import Swinnerton.R")
source("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Image Analysis/import Italian Gulch.R")

bs <- bs.fn()
dc <- dc.fn()
s <- s.fn()
ig <- ig.fn()

# Stick them all together for now... 
all <- bind_rows(bs, dc, s, ig)
all <- select(all, -Moon.Phase, -Illum, -Label, -Contrast, -Brightness, -Sharpness, -Saturation, -Sensitivity,
              -Battery.Volts., -Image.Quality)

# Fill in blank op.state as "operating"
all$op.state[all$op.state == ""] <- "operating"
all$op.state[all$op.state == "Takedown"] <- "takedown"
all$op.state[all$op.state == "Setup"] <- "setup"

# Number of camera days
cam2 <- group_by(all, cam) %>%
  summarise(camdays = length(unique(dateLST))) %>%
  summarise(total = sum(camdays))

# Number of pictures with elk present in foreground
elkpics <- length(which(all$present == "elk present")) + 
  length(which(all$elk.present == "Elk event" | all$elk.present == "Elk present"))

# Number of pictures with any animal present in the foreground
anipics <- length(which(all$animals.present == "Animal event" | 
                                      all$animals.present == "Animal present")) +
  length(which(all$present == "other animals present"))
