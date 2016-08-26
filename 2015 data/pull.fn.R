# Pull.fn
# Anna Moeller
# 8/2/2016

### Be really careful about doing this and try to preserve Molly's ddbs

# Write function
pull.fn <- function(from, dest, destbackup){
  # Copy new timelapse files to a destination, and move old ones to a backup
  # Takes:
  #   from: a directory, where the files are coming from
  #   dest: a MATCHING directory, where the files and going to
  #   destbackup: a matching directory, where old files are being moved
  
  # List all the timelapse files in "from"
  files <- list.files(from, pattern = "Timelapse|Template", recursive = T, full.names = T)
  files <- files[grepl("csv|tdb|ddb", files)]
  
  # Create file names for the destination and backup
  destfiles <- gsub(from, dest, files)
  backupfiles <- gsub(dest, destbackup, destfiles)

  # If these files already exist in "dest", move them to the backup
  # This doesn't necessarily move old backup files, but I think it doesn't matter
  if(any(file.exists(destfiles))){

    # Create destbackup
    dir.create(destbackup)
    
    # Create a backup directory for each
    backupdir <- dirname(backupfiles[file.exists(destfiles)])
    sapply(backupdir, dir.create, recursive = T)
    
    # Move those files
    file.rename(destfiles[file.exists(destfiles)], backupfiles[file.exists(destfiles)])
  }

  # Create all the directories in "dest"
  sapply(dirname(destfiles), dir.create, recursive = T)

  # Copy the new files 
  file.copy(files, destfiles)
}


# # Example call (particular camera):
# pull.fn(from = "F:/Camera Trap Photos/2015-16/Beaverhead/AM174/Trip 1/101RECNX",
#         dest = "H:/Camera Trap Photos/2015-16/Beaverhead/AM174/Trip 1/101RECNX",
#         destbackup = paste("H:/Camera Trap Photos/Other/Old Files", Sys.Date(), 
#                            "Beaverhead/AM174/Trip 1/101RECNX", sep = "/")
# )
# 
# from <- trips[c(1:6, 8:10, 12, 21:23, 25:26, 28:29, 31:34, 36:42, 44:46, 49:52, 54, 62, 64, 65, 67, 70:73,
#               76:80, 83:85, 90, 92, 110:112, 115:117, 120:125, 168, 169)]



###################################################################
# # Copy completed Timelapse Files to new harddrive
# pull.fn <- function(from = "G:", to, pattern = "TimelapseData.csv"){
#   # Takes: to: a full directory to copy the files to
#   #        from: the letter of the directory to copy files from
#   #        pattern: the file you are looking for
#   
#   
#   # Initialize the letters and directories that will be used later
#   toletter <- substr(to[1], 1, 2)
#   fromletter <- from
#   from <- gsub("^[A-Z]:", fromletter, to) 
#   
#   # Create a backup directory to move old files to (in the destination directory)
#   backup <- paste(toletter, "Camera Trap Photos/Other/Old Files", 
#                   as.character(Sys.Date()), 
#                   paste(str_extract(to, "AM\\d{2}\\d?"), 
#                         str_extract(to, "Trip [123]"), 
#                         str_extract(to, "10[012]RECNX"), sep = "_"), 
#                   sep = "/")
#   
#   # If file exists in both places, move the old file
#   if(is.null(pattern)) {
#     if(length(list.files(from)) > 0 & length(list.files(to)) > 0) {
#       
#       # Create backup directory
#       if(dir.exists(paste(backup, "Backups", sep = "/")) == F) {
#         dir.create(paste(backup, "Backups", sep = "/"), recursive = T)
#       }
#       file.copy(from = list.files(to, full.names = T),
#                 to = paste(backup, "Backups", sep = "/"))
#       file.remove(list.files(to, full.names = T))
#     }
#   }
#   if(file.exists(paste(to, pattern, sep = "/")) &
#      file.exists(paste(from, pattern, sep = "/"))){
#     
#     # Create backup directory      
#     if(dir.exists(backup) == F) {
#       dir.create(backup, recursive = T)
#     }
#     
#     file.copy(from = paste(to, pattern, sep = "/"),
#               to = backup)
#     file.remove(paste(to, pattern, sep = "/"))
#   }
#   
#   # If the file exists in the new drive, copy it to the old drive
#   if(is.null(pattern)) {
#     if(dir.exists(to) == F) {
#       dir.create(to)
#     }
#     if(length(list.files(from)) > 0){
#       file.copy(from = list.files(from, full.names = T),
#                 to = to)
#     }
#   } else if(file.exists(paste(from, pattern, sep = "/"))){
#     file.copy(from = paste(from, pattern, sep = "/"),
#               to = to)
#   }
# }
# 
# # Run it
# toH <- gsub("^F:", "H:", trips)
# l <- lapply(trips, pull.fn, from = "G:", pattern = "TimelapseData.csv")
# 
# # TimelapseData.csv, TimelapseData.ddb, AKM Template 20160218 (1).tdb, AKM Template 20160218.tdb
# # # Also, copy all my new metadata onto Molly's HD
# # lapply(trips, pull.fn, to = "H:", pattern = "Metadata.csv", to.full.name = T)
# 
# # Test 1 
# from <- "G:"
# to <- "F:/Camera Trap Photos/2015-16/Beaverhead/AM74/Trip 1/100RECNX"
# pattern <- "TimelapseData.csv"
# pull.fn(from = "G:", to = "F:/Camera Trap Photos/2015-16/Beaverhead/AM74/Trip 1/100RECNX",
#         pattern = "TimelapseData.csv")