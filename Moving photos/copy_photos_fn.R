### Anna Moeller
### Copy photos from SD cards
### 2/24/2015
################################################################################
# Copy Reconyx photos from SD card to hard drive 
copy_photos_fn <- function(from = "F:", to = "G:", cam, site, date = as.character(format(Sys.Date(), format = "%m%d%Y"))){
  sdpath <- grep("RECNX", x = list.files(paste(from, "DCIM", sep = "/"), full.names = T), value = T) # Only Reconyx files
  files <- list.files(sdpath, full.names = T)
  moveto <- paste(to, "Camera Trap Photos/2015-16", site, date, cam, sep = "/")
  dir.create(moveto, recursive = T)
  file.copy(from = files, to = moveto, copy.date = T)
  print("Success!")
}
