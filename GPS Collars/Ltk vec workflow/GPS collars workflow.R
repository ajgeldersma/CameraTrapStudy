# Anna Moeller
# GPS Collar Cleanup
# 1/29/2015
################################################################################

# Download Lotek collars for all animals to C:/
#   Log on to https://webservice.lotek.com 
#   username amoeller, password W_____
#   Select all and export to .txt

# Export Vectronic collar from GPS Plus X to C:/
#   Side menu Data > Storage > Default Storage
#      Highlight all, right-click on Positions, export all position data as .csv
#   Put in new folder with today's date

##### Need to check: is my GMT in clean functions correct?
##### Need to check: is my collars 2015 sheet correct?

# Load packages
library(sp)
library(rgdal)
library(dplyr)

# Set working directory
setwd("C:/Users/anna.moeller/Documents/Camera Trap Study/GPS Collars")

# Today's date for file names
today_yy <- as.character(format(Sys.Date(), format = "%m%d%y"))
today_yyyy <- as.character(format(Sys.Date(), format = "%m%d%Y"))

# Clean up and combine Lotek and Vectronic collars
source("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/GPS Collars/GPS collars workflow/ltk_vec_cleanup_fns.R")

# Clean up Lotek
ltk <- clean_ltk_fn(today_yy)

# Clean up Vectronic
vec <- clean_vec_fn(today_yyyy)

# Stick 'em together
# Default inputs: extent = "Idaho", DOPfilt = 6.0, start = "2014-12-01", deploy15 = T
combod <- ltk_vec_combo_fn()

# Delete points before the animal was captured
elk <- combod %>%
  filter(serial != "17604",
         serial != "17620",
         serial != "17621",
         serial != "17658") %>%
  group_by(serial) %>%
  do(delete_cap(.)) %>%
  ungroup()

#  Workspace housekeeping
rm(list = c("combod", "vec", "ltk", "today_yy", "today_yyyy"))
gc()

# Make it spatial and write a shapefile
source("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/GPS Collars/GPS collars workflow/spatial_fn.R")
# Call, inputs: data, date_yyyy
spatial_fn(elk2, today_yyyy)
