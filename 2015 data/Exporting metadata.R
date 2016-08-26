# Reading in Metadata
# Anna Moeller
# 3/17/2016

# Download perl, then 
# devtools::install_github("ironholds/exif") # This one didn't work
# devtools::install_github("paleolimbot/exifr")

# Load packages
library(exifr)

get_meta_fn <- function(dir){
  files <- list.files(path = dir, pattern = ".*JPG$", full.names = T)
  meta <- exifr(files)
  write.csv(meta, file = paste(dir, "Metadata.csv", sep = "/"))
  print("Success!")
}


# # Start the clock
# start <- Sys.time()
# # Call the function
# cams <- list.files(pattern = "^AM.*")
# lapply(cams, get.meta.fn)
# # Stop the clock
# Sys.time() - start

# # Started at 1:43 pm, ended at 2:06 pm. Took 23 minutes.
# # Make sure to save this one. 
# write.csv(meta, file = "C:/Users/anna.moeller/Pictures/RECONYX Images/Beaverhead 2015_16/AM23/AM23 Metadata.csv")
# 
# # Read it back in, see if it worked
# tosee <- read.csv("C:/Users/anna.moeller/Pictures/RECONYX Images/Beaverhead 2015_16/AM23/AM23 Metadata.csv")

# # Read them back in
# dir <- paste(cams, "/", cams, " Metadata.csv", sep = "")
# tst <- lapply(dir, read.csv)
