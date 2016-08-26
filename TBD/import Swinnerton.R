# Import Swinnerton
# Anna Moeller
# 7/9/2015

s.fn <- function(){
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
}