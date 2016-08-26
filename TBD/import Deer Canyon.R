# import Deer Canyon
# Anna Moeller
# 7/21/2015

dc.fn <- function(){
 
  # Initialize region and site
  region <- "Beaverhead Reg. 6" 
  site <-"DC" 
  
  # Set working directory
  setwd(paste("C:/Users/anna.moeller/Pictures/RECONYX Images", region, sep = "/"))
  
  # List files
  files <- grep(site, list.files(), value = T)
  
  # Call combo_fn, inputs: region, site, cam
  source("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Image Analysis/combo_fn_TIA_format_2.R")
  
  # For Beaverhead Reg. 6/Deer Canyon
  f1 <- combo_fn(region, site, cam = "DC AM41")
  f23 <- do.call(rbind, lapply(files[2:3], combo_fn, region = region, site = site))
  f4 <- combo_fn(region, site, cam = "DC AM45")
  f5678 <- do.call(rbind, lapply(files[5:8], combo_fn, region = region, site = site))
  
  # Make them all the same.
  f1 <- mutate(f1, present = elk.present) %>%
    select(-bird, -elk.present)
  f123 <- rbind(f1, f23)
  f123 <- select(f123, -uncounted)
  f4 <- select(f4, -background)
  f1234 <- rbind(f123, f4)
  f5678 <- select(f5678, -background, -sheep, -cattle, -horse)
  dc <- rbind(f1234, f5678)

  # Need to fix capitalization within the columns
  dc$op.state[dc$op.state == "Operating"] <- "operating"
  dc$op.state[dc$op.state == "Setup"] <- "setup"
  dc$op.state[dc$op.state == "Snow"] <- "snow"
  dc$op.state[dc$op.state == "Takedown"] <- "takedown"
  
  dc$present[dc$present == "Elk present"] <- "elk present"
  dc$present[dc$present == "Humans present"] <- "humans present"
  dc$present[dc$present == "No animals present"] <- "no animals present"
  dc$present[dc$present == "Other animals present"] <- "other animals present"
  dc$present[dc$present == "" & dc$op.state == "setup"] <- "humans present"
  
  # There are still two records where present is blank
  
return(dc)
}
