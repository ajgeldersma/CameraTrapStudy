# Combine metadata and timelapse data
# Anna Moeller
# 3/30/2016

# Combines Reconyx metadata with Timelapse Image Analyzer data (version 2.0) for 2015 data
combo_timelapse3_fn <- function(dir){
  # Works for one folder of picures. Use lapply to do over all photos
  
  # For debugging
  print(dir)
  
  # See if both the metadata and the timelapse csv exist
  exist.meta <- list.files(dir, pattern = "Metadata.csv", full.names = T)
  exist.TIA <- list.files(dir, pattern = "TimelapseData.csv", full.names = T)
  
  # If they exist go ahead. Otherwise, just give me the metadata
  if(length(exist.meta) == 1 & length(exist.TIA) == 1){
    
    # Check that both files are the same length
    meta <- read.csv(exist.meta, as.is = T)
    TIA <- read.csv(exist.TIA, as.is = T)
    stopifnot(dim(meta)[1] == dim(TIA)[1])
    
    # Pick the metadata columns I want to keep
    source("GitHub/CameraTrapStudy/2015 data/make_meta_pretty.fn.R")
    meta <- make_meta_pretty.fn(meta)
    
    # Delete things I don't want from Timelapse (including invisible fields)
    TIA <- select(TIA, -Date, -Time, -ImageQuality, -Folder, -MarkForDeletion,
                  -YOYcutoff, -LionAdult, -LionKitten, -WolfAdult, -WolfPup, 
                  -BlackBearAdult, -BlackBearCub, -CattleCow, -CattleCalf) %>%
      mutate(maxdist = as.character(maxdist))
   
    # If they do exist and are the same, stick them together
    combo <- inner_join(meta, TIA, by = c("FileName" = "File"))
    stopifnot(dim(combo)[1] == dim(meta)[1] & dim(combo)[1] == dim(TIA)[1])
    
    # Make the other column into wide format so we can make a whole long dataframe
    combo$otherwhat[combo$otherwhat == " " | combo$otherwhat == "" | 
                      is.na(combo$otherwhat)] <- "novalue"
    wider <- spread(combo, key = otherwhat, value = other, fill = 0)
    
    # Make sure all the logicals are actually logical
    wider <- mutate(wider,
                   review = as.logical(toupper(substr(review, 1, 1))),
                   greatpic = as.logical(toupper(substr(greatpic, 1, 1))),
                   elkpresent = as.logical(toupper(substr(elkpresent, 1, 1))),
                   deerpresent = as.logical(toupper(substr(deerpresent, 1, 1))),
                   prongpresent = as.logical(toupper(substr(prongpresent, 1, 1))),
                   humanpresent = as.logical(toupper(substr(humanpresent, 1, 1))),
                   otherpresent = as.logical(toupper(substr(otherpresent, 1, 1))),
                   uniquemark = as.logical(toupper(substr(uniquemark, 1, 1))))
   return(wider)
    
  } else { 
    # Issue a warning that the TIA file does not exist
    warning("Timelapse file does not exist. Returning metadata only")
    
    # Read in metadata
      meta <- read.csv(exist.meta, as.is = T)
    
    # Pick the metadata columns I want to keep
      source("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/2015 data/make_meta_pretty.fn.R")
      meta <- make_meta_pretty.fn(meta)

    return(meta)
  }
}