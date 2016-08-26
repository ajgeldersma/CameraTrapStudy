# IDFG Deer GPS Data Cleanup
# 11/21/2014

# Load packages and data
library(stringr)
library(sp)
library(rgdal)
deer <- read.csv("F:/IDFG/GPSdeerlocs122013/GPSdeerlocs122013.csv", as.is = T)
# as.is = T keeps characters and integers not factors
# sp and rgdal for spatial stuff

################################################################################
# Tag 8554 came with year 1923, script converts to 2019. Just be 
#     aware that this is an issue 
################################################################################

# Drop non-date numbers in date column
foo <- function(x) gsub("([01]?[0-9]/[0-3]?[0-9]/2?0?[01][0-9]).*", "\\1", x)
deer$date <- foo(deer$date)
# Format all years in 4 digits
goo <- function(x) gsub("([01]?[0-9]/[0-3]?[0-9])/([01][0-9])", "\\1/20\\2", x)
deer$date <- goo(deer$date)
# Format date column as time 
deer$date <- as.POSIXct(strptime(deer$date, "%m/%d/%Y"))
# Overwrite original month column
deer$month <- as.numeric(format.POSIXct(deer$date, "%m"))
# Turn lat/long into spatial column (xysp is an example only)
xysp <- SpatialPointsDataFrame(deer[,c("long", "lat")])
xyspdf <- SpatialPointsDataFrame(deer[,c("long", "lat")], 
                               data = deer,
          proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" 
))
#  Transform "projection" to IDTM
xyspdf2 <- spTransform(xyspdf, CRS("+proj=tmerc +lat_0=42 +lon_0=-114 +k=0.9996 +x_0=2500000 +y_0=1200000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0"))
#  Write shapefile
writeOGR(xyspdf2, dsn = "F:/IDFG/GPSdeerlocs122013", layer = "idfgdeer", driver = "ESRI Shapefile")


################################################################################
# Other things I worked on that I no longer need in this script
# Format all dates as 1/1/2008
format2 <- function(x) gsub("^0([1-9])/0([1-9])/", "\\1/\\2/", x)
deer$date <- format2(deer$date)

# Subset date column where date is greater than today
deer$date[deer$date > Sys.time()]

# Josh was not familiar with transform
deer2 <- transform(deer, date = as.character(date))
# Transform back into date format
deer6 <- transform(deer5, date = as.POSIXct(date, "%m/%d/%Y", tz = "MST"))
# I made up MST. I have no idea what time zone this is.
# Check for NAs
table(deer6$date)
# 2019 is from 1923 from tag 8554

# Other methods that would be cleaner:
deer$date <- as.character(deer$date)
deer5$date <- as.POSIXct(deer5$date, "%m/%d/%Y", tz = "MST")

# Other methods I tried that didn't work
# deer3 <- gsub("([0-1]?[0-9]/[0-3]?[0-9]/2?0?[01][0-9]).*", "\\1", deer2$date)
# deer3 <- replace(deer2$date, "([0-1]?[0-9]/[0-3]?[0-9]/2?0?[01][0-9]).*", "\\1")
# Both of these turn deer3 into a vector
# deer3 <- gsub... replaced NA with \\1
# deer3 <- gsub(".*1923.*", NA, deer2$date)

# Pull out date column
# date <- as.character(deer$date)
# date <- str_match(deer$date, "[0-1]?[0-9]/[0-3]?[0-9]/20[01][0-9]")
# date2 <- as.POSIXct(date, "%m/%d/%Y", tz = "MST")
### This pulls out only the dates that work, what happens when I try to 
###   reconnect it to unique ID? How can I pull out the good dates + NA?