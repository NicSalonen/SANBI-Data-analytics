importSurvey <- function(filepath, filename, filetype = NULL){

################################################################################
### Set working directory ######################################################
################################################################################

    setwd(filepath)

### Defaults####################################################################

	if (grepl('.gpx', filename, fixed = TRUE)){
		if (is.null("filetype")){
			filetype <- 'track'
				warning('filetype defaulted to "track", if this is a waypoint file,
					set filetype to "waypoint".')
		}
	}

################################################################################

    if (grepl('.csv', filename, fixed = TRUE)){
        path <- file.path(filepath, filename)
        trackcsv <- read_csv(path, skip = 52) ## possibly add lines to skip as argument
        lat <- trackcsv$lat
        lon <- trackcsv$lon
        track <- data.frame(lat,lon)

    } else if (grepl('.gpx', filename, fixed = TRUE)){
        path <- file.path(filepath, filename)
        if (grepl('track', filetype, fixed = TRUE)) {
            trackgpx <- readGPX(path, metadata = FALSE, tracks = TRUE,
                                waypoints = FALSE, routes = FALSE,
                                bounds =  FALSE)
            tracks <- trackgpx[["tracks"]]
            lat_list <- vector(mode = "list", length = length(tracks))
            lon_list <- vector(mode = "list", length = length(tracks))

            for(n in 1:length(tracks)){
                lat_list[[n]] <- tracks[[n]][[1]][["lat"]]
                lon_list[[n]] <- tracks[[n]][[1]][["lon"]]
            }
            lat <- unlist(lat_list)
            lon <- unlist(lon_list)

        } else if (grepl('waypoint', filetype, fixed = TRUE)) {
            trackgpx <- readGPX(path, metadata = FALSE,
                                waypoints = TRUE, tracks = FALSE,
                                routes = FALSE, bounds =  FALSE)
            tracks <- trackgpx[["waypoints"]]
            lat <- tracks$lat
            lon <- tracks$lon
        }
        track <- data.frame(lat, lon)

    } else {
        stop('importSurvey only accepts .csv or .gpx file formats')
    }

    message("END OF IMPORTSURVEY")
    message("Latitude and longitude data from ", filename,
            " has been imported to R!")

    if (filetype == "track") {
        message("Number of track logs: ",
                toString(length(trackgpx[["tracks"]])))
    } else if (filetype == "waypoint") {
        message("Number of waypoints: ", toString(nrow(track)))
    }

    return(track)

}
### END OF IMPORTSURVEY ########################################################


