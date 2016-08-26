# Import and format all the Boulder Spring files
# Anna Moeller
# 7/21/2015

bs.fn <- function(){
  
  # Initialize region and site
  region <- "Beaverhead Reg. 6" 
  site <-"BS" 
  
  # Set working directory
  setwd(paste("C:/Users/anna.moeller/Pictures/RECONYX Images", region, sep = "/"))
  
  # list files
  files <- grep(site, list.files(), value = T)
  
  # Call combo_fn, inputs: region, site, cam
  source("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Image Analysis/combo_fn_TIA_format_2.R")
  f1 <- do.call(rbind, lapply(files[1:6], combo_fn, region = region, site = site))
  
  # Call combo_fn, inputs: region, site, cam
  source("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Image Analysis/combo_fn_TIAv2.R")
  f78 <- do.call(rbind, lapply(files[7:8], combo_fn, region = region, site = site))
  f9 <- combo_fn(region, site, cam = files[9])
  
  # Make them all the same.
  f9 <- select(f9, -review)
  f789 <- rbind(f78, f9)
  f2 <- mutate(f789, op.state = opstate, collar.yn = collaryn, animal.other = animalother, 
               animal.unkn = animalunkn, Image.Quality = ImageQuality) %>%
    select(-MarkForDeletion, -opstate, -collaryn, -bird, -animalother, -animalunkn, -ImageQuality)
  bs <- rbind(f1, f2)
  
  # Need to fix capitalization within the columns
  bs$present[bs$present == "Elk present"] <- "elk present"
  bs$present[bs$present == "Humans present"] <- "humans present"
  bs$present[bs$present == "No animals present"] <- "no animals present"
  bs$present[bs$present == "Other animals present"] <- "other animals present"
  
  bs$op.state[bs$op.state == "Operating"] <- "operating"
  bs$op.state[bs$op.state == "Setup"] <- "setup"
  bs$op.state[bs$op.state == "Snow"] <- "snow"
  bs$op.state[bs$op.state == "Takedown"] <- "takedown"
  
  bs$background[bs$background == "PH"] <- "ph"
  
  return(bs)
}