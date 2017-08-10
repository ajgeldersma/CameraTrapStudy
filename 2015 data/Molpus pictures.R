  # Molpus pictures
  library(dplyr)
  load("C:/Users/anna.moeller/Documents/GitHub/CameraTrapStudy/2015 data/pics.wide20160804.RData")
  
  mol <- pics %>%
    filter(cam %in% c("AM144", "AM132", "AM118", "AM139", "AM98"), 
           humanpresent == F, 
           elkpresent == T | deerpresent == T | otherpresent == T)

  # All files
  from <- mol$SourceFile
  from <- gsub("^F:", "H:", from)
  
  # Just force it
  ff <- from[grep("AM98", from)]
  to <- "F:/Photos AKM 2015-16/AM98"
  file.copy(ff, to)
  
  f2 <- from[grep("AM132", from)]
  t2 <- "F:/Photos AKM 2015-16/AM132"
  file.copy(f2, t2)
  
  f3 <- from[grep("AM139", from)]
  t3 <- "F:/Photos AKM 2015-16/AM139"
  file.copy(f3, t3)
  
  f4 <- from[grep("AM144", from)]
  t4 <- "F:/Photos AKM 2015-16/AM144"
  file.copy(f4, t4)
  
  # ff <- from[1:2]
  # tt <- c("F:/Photos AKM 2015-16/AM118/RCNX0001.JPG",
  #         "F:/Photos AKM 2015-16/AM118/RCNX0002.JPG")
  # 