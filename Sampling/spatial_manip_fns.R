		#  Spatial manipulation functions
		#  Josh Nowak
		#  07/2015
#################################################################################
		xy2zone <- function(xy){
			#  Takes a two column matrix or data.frame with columns x and y and 
			#  projected as:
			#   +proj=tmerc +lat_0=42 +lon_0=-114 +k=0.9996 +x_0=2500000 +y_0=1200000 +datum=NAD83 +units=m +no_defs
		  #   +ellps=GRS80 +towgs84=0,0,0 
			#  OR a spatial points object that can be projected using xy_prep_fun
			#  Returns a vector representing the zone each point falls in
			#  Internally loads zone reference shape from data folder
      zones <- readOGR("C:/Users/anna.moeller/Documents/GIS Layers/IDFG Elk Management Zones", "ElkManagementZones")
      
# 			#  XY must be spatial, make it so if not
# 			if(class(xy) != "SpatialPoints"){
# 				xy <- xyprep_fun(xy, zones)
# 			}
# 			
			#  The projection, units, etc must be the same, make it so and 
			#  extract values of units
			if(identicalCRS(xy, zones)){
				out <- over(xy, zones)$NAME
			}else{
				newxy <- spTransform(xy, CRS(proj4string(zones)))
				out <- over(newxy, zones)$NAME
			}
		return(out)
		}
		#
		xy2gmu <- function(xy){
			#  Takes a two column matrix or data.frame with columns x and y and 
			#  projected as:
		  #   +proj=tmerc +lat_0=42 +lon_0=-114 +k=0.9996 +x_0=2500000 +y_0=1200000 +datum=NAD83 +units=m +no_defs
		  #   +ellps=GRS80 +towgs84=0,0,0 
			#  OR a SpatialPoints object that can be projected using xy_prep_fun
			#  Returns a vector representing the unit each point falls in
			#  Internally loads unit reference shape from data folder
		  gmus <- readOGR("C:/Users/anna.moeller/Documents/GIS Layers/IDFG Game Management Units", "GameManagementUnits")
		  
			#  XY must be spatial, make it so if not
			#if(class(xy) != "SpatialPoints"){
			#	xy <- xyprep_fun(xy, units)
			#}
			
			#  The projection, units, etc must be the same, make it so and 
			#  extract values of units
			if(identicalCRS(xy, gmus)){
				out <- over(xy, gmus)$NAME
			}else{
				newxy <- spTransform(xy, CRS(proj4string(gmus)))
				out <- over(newxy, gmus)$NAME	
			}
		return(as.character(out))
		}
		#
		xyprep_fun <- function(xy, target){
			#  Takes matrix or data.frame with columns x and y stored as 
			#  decimal degrees and projected WGS84 like
			#  Returns a spatial points object projected to
			#  "+proj=utm +zone=14 +datum=NAD83 +units=m +no_defs +ellps=GRS80 
			#   +towgs84=0,0,0"
			initial_p4s <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
			tmp <- SpatialPoints(xy, initial_p4s)
			out <- spTransform(tmp, CRS(proj4string(target)))
		return(out)			
		}
		#  
		spsub_fun <- function(xy){
			#  Takes longitude and latitude values in decimal degrees WGS84
			#  Returns a spatial subset of the data conforming to the state of SD
			#  data.frame in and out, no class objects received or returned
			out <- xy %>%
					filter(Latitude > 42 & Latitude < 46 & 
							Longitude > -105 & Longitude < -96)
		return(out)
		}
		#
		get_spref <- function(x){
			#  A function to get spatial reference information from 2014, the
			#  function operates on lat and long when present otherwise simply
			#  uses the unit to get unit and region...missing data deleted
			
			#  Takes a data frame with at least Unit and Region columns
			#  Returns data frame with unit and region cross referenced to 2014
			#  The function will give preference to lat long when present
			
			if(any(grepl("Lat", colnames(x), ignore.case = T))){
				#  Derive unit and region from lat long if present  
				xy <- x %>%
						spsub_fun(.) %>%
						mutate(Unit = xy2unit(cbind(Longitude, Latitude))) %>%
						mutate(Region = xy2region(cbind(Longitude, Latitude)))%>%
						filter(!is.na(Unit) & !is.na(Region))

				#  Cross reference unit with 2014 when no lat long present
				out <- anti_join(x, xy, by = "ID") %>%
						filter(!is.na(Unit)) %>%
						mutate(Unit2 = cross_ref$ref_unit[
											match(Unit, cross_ref$old)],
								Region = cross_ref$ref_region[
											match(Unit, cross_ref$old)])%>%
						rbind_list(xy, .) %>%
						filter(!is.na(Unit) & !is.na(Unit))
			}else{
				#  If no lat long simply cross ref with 2014
				out <- x %>%
						mutate(Unit2 = cross_ref$ref_unit[
											match(Unit, cross_ref$old)],
								Region = cross_ref$ref_region[
											match(Unit, cross_ref$old)]) %>%
						filter(!is.na(Unit) & !is.na(Unit))
			}
		return(out)
		}
