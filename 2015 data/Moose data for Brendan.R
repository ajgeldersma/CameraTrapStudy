  # Moose data for Brendan
  # Anna Moeller
  # 11/29/2017

  source("GitHub/packages.R")

  # Load picture database
  load("GitHub/CameraTrapStudy/2015 data/pics.wide20160804.RData")
  
  # Moose 
  mm <- pics %>%
    select(site, plot, cam, timeLST, dateLST, 
           grep("moose", names(.), ignore.case = T),
           easting, northing) %>%
    mutate(anymoose = MooseBull + MooseAntlerless + MooseCalf + MooseUnkn) %>%
    filter(anymoose > 0) %>%
    select(-anymoose)

  saveRDS(mm, "GitHub/CameraTrapStudy/2015 data/moose.rds")
  
  # Garmin datum = WGS84
  # We're in UTM zone 11
  mm.sp <- SpatialPointsDataFrame(coords = mm[, c("easting", "northing")],
                                  data = mm, 
                                  proj4string = CRS("+proj=utm +zone=11 +datum=WGS84")
  )
  id <- readOGR("GIS Layers/Idaho border", "idaho")
  id2 <- spTransform(id, mm.sp@proj4string)
  plot(id2)
  plot(mm.sp, add = T)
  