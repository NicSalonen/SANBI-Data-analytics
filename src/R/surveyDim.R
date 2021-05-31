surveyDim <- function(...){
################################################################################	
### Check if there is atleast 1 variable called in the function ################
    
    print(match.call())
    nargin <- length(as.list(match.call())) - 1
    print(nargin)
    
    if (nargin < 1){
        stop("Atleast one set of lat/lon coordinates are required.")
    }
    
################################################################################    
### Compile multiple lat/lon arrays into one and compute site dimensions
    
    if (nargin == 1){
        varargin <- (...)
    } else if (nargin > 1){
         varargin <- rbind(...)
    }

    ## Haversine Calculation
    # Convert to radians
    lat_rad <- varargin[["lat"]]*(pi/180)
    lon_rad <- varargin[["lon"]]*(pi/180)
    
    # Delta lat and delta lon
    dlon_rad <- max(lon_rad) - min(lon_rad)
    dlat_rad <- max(lat_rad) - min(lat_rad)
    
    # Haversine Formula
    # Latitudinal distance
    Alat <- sin(dlat_rad/2)^2 + cos(max(lat_rad))*cos(min(lat_rad))*sin(0/2)^2
    Clat <- 2 * atan2(sqrt(Alat), sqrt(1-Alat))
    Dlat <- (6371 * Clat) * 1000
    lat_dist <- round(Dlat,0)
    
    # Longitudinal distance
    Alon <- sin(0/2)^2 + cos(max(lat_rad))*cos(min(lat_rad))*sin(dlon_rad/2)^2
    Clon <- 2 * atan2(sqrt(Alon), sqrt(1-Alon))
    Dlon <- (6371 * Clon) * 1000
    lon_dist <- round(Dlon, 0)
    
    srvydetails <- list(varargin, lat_dist, lon_dist)
    
    message("END OF SURVEYDIM")	
    if (nargin == 1){
        message("Survey site dimensions have been calculated based on the haversine equation.")
    } else {
        message("Survey site dimensions have been calculated based on the haversine equation and all tracks have been concatenated.")
    }
    
return(srvydetails)
    
}


