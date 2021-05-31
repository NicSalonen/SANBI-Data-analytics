surveyEfficacy <- function(srvydetails, surveymap, bins){
## Generate Survey Efficacy coefficient
# Area of each grid cell/bin = bin_width^2
# Calculate precise bin area
track <- srvydetails[[1]]
lat_bins <- surveymap[[2]][[2]]
lon_bins <- surveymap[[2]][[1]]
wp <- surveymap[[3]]
surveymap <- surveymap[[1]]


co.dec <- SpatialPoints(cbind(track$lon, track$lat), proj4string = CRS("+proj=longlat"))
utm_co <- spTransform(co.dec, CRS("+init=epsg:32734"))
#utm_co

bin_height <- (max(utm_co@coords[,1] - (min(utm_co@coords[,1])))/lat_bins)
bin_length <- ((max(utm_co@coords[,2]) - (min(utm_co@coords[,2])))/lon_bins)
bin_area <- bin_length*bin_height

area_missed <- as.data.frame(table(surveymap[surveymap == 0]))
area_missed <- area_missed$Freq*bin_area
area_viewed <- as.data.frame(table(surveymap[surveymap == 0.5]))
area_viewed <- area_viewed$Freq*bin_area
area_visited <- as.data.frame(table(surveymap[surveymap >= 1]))
area_visited <- sum(area_visited$Freq[area_visited$Freq >= 1])
area_visited <- area_visited*bin_area

if (!is.null(wp)) {
  num_obs <- nrow(wp)
} else {
  num_obs <- 0
}

area_total <- area_missed + area_viewed + area_visited

# Percentage of area surveyed/viewed/missed
perc_missed <- (area_missed/area_total)*100
perc_visited <- (area_visited/area_total)*100
perc_viewed <- (area_viewed/area_total)*100
perc_surveyed <- ((area_viewed+area_visited)/area_total)*100

# compute shortest dist

# Get shortest distances from observations to missed
if (!is.null(wp)){
  obs <- which(surveymap == 2, arr.ind = TRUE)
  mis <- which(surveymap == 0, arr.ind = TRUE)

  shortest <- vector()
  permissedpoint <- vector()
  for (d in 1:nrow(obs)){
      for (dd in 1:nrow(mis)){
         permissedpoint[dd] <- sqrt((obs[d,1]- mis[dd,1])^2 + (obs[d,2] - mis[dd,2])^2)
     }
     shortest[d] <- min(permissedpoint)
     permissedpoint[] <- 0
  }

  shortestDist <- min(shortest)
} else {
  shortestDist <- "No waypoint file"
}

if (length(num_obs) == 0){
  num_obs <- 0
  efficacy <- data.frame(perc_surveyed, perc_visited, perc_viewed, perc_missed, num_obs, shortestDist)
} else {
  efficacy <- data.frame(perc_surveyed, perc_visited, perc_viewed, perc_missed, num_obs, shortestDist)
}
names(efficacy) <- c("% Surveyed", "% Visited", "% Viewed", "% Missed", "# Observations", "Shortest Dist [m]")

return(efficacy)

}
