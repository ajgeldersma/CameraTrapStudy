# Make spatial
# Anna Moeller
# 5/19/2015

spatial_fn <- function(x, date_yyyy){
  # Make spatial
  elksp <- SpatialPointsDataFrame(x[, c("long", "lat")], 
                                      data = x,
                                      proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  # Project in IDTM
  elkIDTM <- spTransform(elksp, CRS("+proj=tmerc +lat_0=42 +lon_0=-114 +k=0.9996 +x_0=2500000 +y_0=1200000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0"))

  # Write shapefile
  writeOGR(elkIDTM, dsn = "Shapefiles", layer = paste("idfgelk", date_yyyy, sep = ""), 
         driver = "ESRI Shapefile")
  print("shapefile written to wd")
}