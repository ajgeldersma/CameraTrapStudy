# Anna Moeller
# All Elk collars from Mike Elmer 
# 1/8/15
################################################################################
# Read in data
raw <- read.table("F:/IDFG/Elk Locations MElmer 01082015.txt", header = T,
                  sep = ",", as.is = T)
# Drop all rows that have NAs in LAT or LONG and are not near Idaho
points <- raw[complete.cases(raw[, 4:5]), ]
points2 <- points[points$LAT > 42 & points$LAT < 49 & points$LONG > -118 & 
                    points$LONG < -111, ]
# Make spatial
library(sp)
library(rgdal)
rawsp <- SpatialPointsDataFrame(points2[, c("LONG", "LAT")], 
                                 data = points2,
                                 proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" 
                                 ))
# Project in IDTM
rawsp2 <- spTransform(rawsp, CRS("+proj=tmerc +lat_0=42 +lon_0=-114 +k=0.9996 +x_0=2500000 +y_0=1200000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0"))
# Put Date in date format
rawsp2$GMT <- as.POSIXct(strptime(rawsp2$GMT, tz = "GMT", "%m/%d/%Y %H:%M:%S"))
rawsp2$LMT <- as.POSIXct(strptime(rawsp2$LMT, tz = "GMT", "%m/%d/%Y %H:%M:%S"))


# Load time zone map #### I don't like this map. 
require(maptools)
shape <- readShapePoly("F:/GIS Layers/TZ_Idaho.shp")
raw.tz <- rawsp2[!is.na(overlay(rawsp2, shape)),]
### This is deprecated...?

panh <- # points that are in northern polygon
beavr <- # points that are in southern polygon
# Format LMT 
panh$LMT <- format(c$GMT, tz="US/Pacific", usetz=TRUE)
beavr$LMT <- format(c$GMT, tz="US/Mountain", usetz=TRUE)




# Match only collars that belong to elk
collars <- read.csv("F:/Camera Trap Study/Marking collars/Shape assignment.2.csv", 
                    header = T, as.is = T)
elk <- rawsp2[rawsp2$DeviceID %in% collars$Collar.serial, ]


# Select points in timeframe I'm interested in 
# Filter out DOP (?)

#  Write shapefile
writeOGR(elk, dsn = "F:/IDFG", layer = "idfgelk01092015", driver = "ESRI Shapefile")
