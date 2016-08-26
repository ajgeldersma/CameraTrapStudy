# import Italian Gulch
# Anna Moeller
# 7/9/2015

ig.fn <- function(){
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
}
