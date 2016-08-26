# Workflow for Panhandle collar comparison
# Anna Moeller
# 7/9/2015

# Load packages
library(dplyr)

##################################
# Bring in the data for Swinnerton
##################################

# Initialize region and site
region <- "Panhandle" 
site <-"S" 

# Set working directory
setwd(paste("C:/Users/anna.moeller/Pictures/RECONYX Images", region, sep = "/"))

# List camera names
cam <- grep(paste(site, "AM", sep = " "), list.files(), value = T)

# Call combo_fn, inputs: region, site, cam
source("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Image Analysis/combo_fn_TIA_format_2.R")
pretty <- do.call(rbind, lapply(cam, combo_fn, region = region, site = site))

# Select records where a collar was seen
collpic <- filter(pretty,
                  collar.yn != "") %>%
           select(collar.yn, collar1, collar2, collar3, comments, cattle.tag, 
                  elkcalf, elkcow, elkbull, elkunkn, cam)

# Kill no design and partially visible, make a single column for pattern
colldsn <- filter(collpic, 
                  collar.yn != "Elk No Design",
                  collar.yn != "Elk Partially Visible") %>%
           mutate(collar = paste(collar1, collar2, collar3))

# Are any of the cattle tags seen twice? 
table(colldsn$cattle.tag)

# Are any of the designs seen twice? 
table(colldsn$collar)

# If the collar was seen twice, does everything match up? 

# Load shape assignment 
shapes <- read.csv("C:/Users/anna.moeller/Documents/Camera Trap Study/Marking collars/shape assignment for R.csv")

# Narrow it down to just Panhandle collars
shapes <- filter(shapes, study.area == region)

##########################################################
# Do these designs and cattle tags match with the database? 
##########################################################

# triangle heart unknown, unknown
shapes[shapes$collar1 == "triangle" & shapes$collar2 == "heart", ] # Exists, but could be any.

# triangle heart circle, 23 (seen in full twice)
shapes[shapes$design == "triangle heart circle", ] # YES!

# triangle circle triangle (seen once no cattle tag, seen once 25)
shapes[shapes$design == "triangle circle triangle", ] # WRONG, not deployed. uh-oh, I saw this twice... 

# heart unknown unknown, unknown
shapes[shapes$collar1 == "heart", ] # Exists, but could be any.

# unknown moon unknown, 90
shapes[shapes$collar2 == "moon", ] # Exists, but none have cattle tag 90
shapes[shapes$cattle.tag == 90, ] # heart heart moon. WRONG

# triangle circle unknown, 25
shapes[shapes$collar1 == "triangle" & shapes$collar2 == "circle", ] # Exists, but none have cattle tag 25.

# triangle square circle, 46
shapes[shapes$design == "triangle square circle", ] # WRONG, not deployed. 
shapes[shapes$cattle.tag == 46, ] # triangle moon circle. 

# What's the real identity of cattle tag 25? (seen 3 times)
shapes[shapes$cattle.tag == 25, ] # heart circle heart

# Out of 9 pictures of partially identified patterns, I got 2 (1 collar) right




# Some of these are elk present, some are elk event

#####################################
# Bring in the data for Italian Gulch
#####################################

# Initialize region and site
region <- "Panhandle" 
site <- "IG"

# Set working directory
setwd(paste("C:/Users/anna.moeller/Pictures/RECONYX Images", region, sep = "/"))

# List camera names
cam <- grep(paste(site, "AM", sep = " "), list.files(), value = T)

# Call combo_fn, inputs: region, site, cam
source("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Image Analysis/combo_fn_TIA_format_2.R")

# Two different formats
f12 <- do.call(rbind, lapply(cam[1:2], combo_fn, region = region, site = site))
f39 <- do.call(rbind, lapply(cam[3:9], combo_fn, region = region, site = site))

# Bind them together
f1 <- select(f12, 
             -animals.present)
f3 <- mutate(f39, 
             cattle.tag = paste(cattle1, cattle2, sep = "")) %>%
      select(-cattle1, -cattle2)
pretty <- rbind(f1, f3)


# Select records where a collar was seen
collpic <- filter(pretty,
                  collar.yn != "") %>%
select(collar.yn, collar1, collar2, collar3, comments, cattle.tag, 
       elkcalf, elkcow, elkbull, elkunkn, cam)

# Kill no design and partially visible, make a single column for pattern
colldsn <- filter(collpic, 
                  collar.yn != "Elk No Design",
                  collar.yn != "Elk Partially Visible") %>%
           mutate(collar = paste(collar1, collar2, collar3))

# Load shape assignment 
shapes <- read.csv("C:/Users/anna.moeller/Documents/Camera Trap Study/Marking collars/shape assignment for R.csv")

# Narrow it down to just Panhandle collars
shapes <- filter(shapes, study.area == region)

#####################
# Check out how I did
#####################

# triangle heart circle, 2?
shapes[shapes$design == "triangle heart circle", ] # Exists, cattle tag 23
shapes[shapes$cattle.tag == 20 | shapes$cattle.tag == 21 | shapes$cattle.tag == 22 | 
         shapes$cattle.tag == 23 | shapes$cattle.tag == 24 | shapes$cattle.tag == 25 | 
         shapes$cattle.tag == 26 | shapes$cattle.tag == 27 | shapes$cattle.tag == 28 | 
         shapes$cattle.tag == 29, ] # 5 of these are triangle heart something

# triangle moon circle, 46
shapes[shapes$design == "triangle moon circle", ] # YES!!!

# unknown moon circle, 46
shapes[shapes$collar2 == "moon" & shapes$collar3 == "circle", ] # YES! The only one it could be is triangle moon circle

# I think I will call getting all of these right.

# What is up with triangle heart circle 23? Why is she posing in BOTH study areas? AM12, AM19, AM21
