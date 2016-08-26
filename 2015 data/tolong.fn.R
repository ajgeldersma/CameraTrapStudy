# Make picture database long
# Anna Moeller
# 8/1/2016

tolong.fn <- function(pics, species){
  # species: elk, md, wtd, prong, human, predator, other
  ### If you do this for deer or any "other", the present column will not be correct
  if(species == "predator"){
    cols <- c(names(pics)[grep("bear|wolf|lion", names(pics), ignore.case = T)],
              "coyote", "bobcat", "fox", "fisher", "grizzly", "lynx")
  } else if(species == "other"){
    cols <- c(names(pics)[grep("cattle|moose|other", names(pics), ignore.case = T)],
              "lagomorph", "turkey", "marten", "domestic sheep", 
              "horse", "unknown")
  } else {
    cols <- names(pics)[grep(species, names(pics), ignore.case = T)]
  }
  pres <- cols[!grepl("cattlenum", cols)] 
  cols <- cols[!grepl("present|cattlenum", cols)]
  out <- select(pics, site, plot, cam, timeLST, dateLST, trigger, viewer, DateProcessed,
                uniquemark, comment, SourceFile, which(names(pics) %in% pres)) %>%
    gather("species", "count", which(names(.) %in% cols))
  return(out)
}