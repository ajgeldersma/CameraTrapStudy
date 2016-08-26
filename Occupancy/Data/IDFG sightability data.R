#  Function to download files
down_file <- function(final_url, method){
  xx <- RCurl::getURL(final_url, ssl.verifypeer = F)
  out <- read.csv(textConnection(xx), as.is = T)
}
#################################################################################
#  Function that actually connects to the API
api_connect <- function(dtype, species, dau){
      #  Base URL
      base_url <- "https://fishandgame.idaho.gov/ifwis/rest/services/wildlife/popmodel/view/"
      
      #  Convert game type
      num_game <- species
      
      final_url <- paste0(base_url, tolower(dtype), "/", "myfile.csv", "?game=", num_game, 
        "&area=", dau)            
        
      #  Connect to API, read data and return R object
      #  Update this line and function arguments to accomodate username 
      #  and passwords once security is setup
      out <- down_file(final_url)  
 
  return(out)
}

# Get sightability data
data <- api_connect("Sightability", 4, 30)

# Questions about the data
# Is this all of the Beaverhead or just unit 30? 
# How do I figure out where the subunits are? 
# How big are subunits? 
# How much land space do they actually fly? 
# How many subunits do they sample? Per stratum? 
# What is the breakdown for strata? Are they equal number of grid cells? 