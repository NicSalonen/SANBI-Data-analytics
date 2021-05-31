plotSurvey <- function(bin_width, srvydetails, waypoint, mapcenter, KEY = NULL, zoom = 16, view = 2){
################################################################################
### Setting defaults and calling errors ########################################

if (!exists('srvydetails') || !exists('waypoint')){
    stop('Input variable srvydetails/waypoint is missing')
}

if (!exists('bin_width')){
    stop('Input variable bin_width is missing')
}

################################################################################
## Get objects from srvydim
track <- srvydetails[[1]]
lat_dist <- srvydetails[[2]]
lon_dist <- srvydetails[[3]]

## Set bin size
lon_bin <- lon_dist/bin_width
lat_bin <- lat_dist/bin_width
bins <- cbind(lon_bin, lat_bin)

# Create gridded GPS track: indicates which Xm^2 areas are covered by
# a surveyor. Data stored as a matrix

zt <- hist2d(track$lon, track$lat, nbins = bins, show = FALSE)
zt$counts[zt$counts >=1] <- 1
zt_counts <- data.matrix(zt$counts)
xseq <- seq(min(zt$x.breaks),max(zt$x.breaks),((max(zt$x.breaks)-min(zt$x.breaks))/lon_bin))
yseq <- seq(min(zt$y.breaks),max(zt$y.breaks),((max(zt$y.breaks)-min(zt$y.breaks))/lat_bin))

if (!is.null(waypoint)){
    zw<- hist2d_breaks(waypoint$lon, waypoint$lat,
                       x.breaks = xseq,
                       y.breaks = yseq,
                       show = FALSE)
    zw$counts[zw$counts >= 1] <- 2
    zw_counts <- data.matrix(zw$counts)

    # Add waypoint count per bin to track grid
    z <- zt_counts + zw_counts
    z[z >= 2] <- 2
} else {
    z <- zt_counts
}

# Assign 'viewed' bins
if (view > 0){
    for (xx in (1+view):(lon_bin-view)){
        for (yy in (1+view):(lat_bin-view)){
            if (z[xx,yy] >= 1){
                for (vv in 1:view){
                    if (z[xx+vv,yy] == 0){
                        z[xx+vv,yy] <- 0.5
                    }
                    if (z[xx-vv,yy] == 0){
                        z[xx-vv,yy] <- 0.5
                    }
                    if (z[xx,yy+vv] == 0){
                        z[xx,yy+vv] <- 0.5
                    }
                    if (z[xx,yy-vv] == 0){
                        z[xx,yy-vv] <- 0.5
                    }
                }
            }
        }
    }
}
# Make 0 values outside of survey area NaN (L to R: essentially top to bottom as before transpose)
for (xa in 1:lon_bin){
    for (xb in 1:lat_bin){
        if (z[xa,xb] == 0){
            z[xa,xb] <- NaN
        } else if (z[xa,xb] > 0){
            break
        }
    }
}
# Make 0 values outside of survey area NaN (R to L: essentially bottom to top)
for (ya in lon_bin:-1:1){
    for (yb in lat_bin:-1:1){
        if (is.nan(z[ya,yb])){
            next
        } else if (z[ya,yb] == 0){
            z[ya,yb] <- NaN
        } else
            break
    }
}

# transpose to traverse grid in another direction
z <- t(z)
# Make 0 values outside of survey area NaN (L to R)
for (xc in 1:lat_bin){
    for (xd in 1:lon_bin){
        if (is.nan(z[xc,xd])){
            next
        }
        else if (z[xc,xd] == 0){
            z[xc,xd] <- NaN
        } else {
            break
        }
    }
}
# Make 0 values outside of survey area NaN (R to L)
for (yc in lat_bin:-1:1){
    for (yd in lon_bin:-1:1){
        if (is.nan(z[yc,yd])){
            next
        }
        else if (z[yc,yd] == 0){
            z[yc,yd] <- NaN
        } else {
            break
        }
    }
}

#transpose to correct orientation
z <- t(z)

#Set survey image settings and colors

pal <- colorRampPalette(c("black", "orange", "darkcyan", "green"))
ck <- list(at = c(0.25, 0.75, 1.25, 1.75), side = 4,
           addlines = TRUE, length = 0.5, width = 0.5, addlines = TRUE,
           labels = c('Missed', 'Viewed', 'Visited', 'Observation'))

# Set google account credentials. Requires API KEY!
lon <- mapcenter[1]
lat <- mapcenter[2]
if (!is.null(KEY) && !is.null(waypoint)) {
    site <- ggmap(get_googlemap(center = c(lon = lon, lat = lat),
                      maptype = 'satellite', zoom = zoom)) +
        geom_path(data = track, aes(x = track$lon, y = track$lat),
                        color = 'red', size = 1) + geom_point(data = waypoint,
                                                              aes(x = waypoint$lon,
                                                                  y = waypoint$lat),
                                                                  color = 'lightgreen',
                                                                  size = 2)

    print(site)

} else if (!is.null(KEY) && is.null(waypoint)){
    site <- ggmap(get_googlemap(center = c(lon = lon, lat = lat),
                                maptype = 'satellite', zoom = zoom)) +
        geom_path(data = track, aes(x = track$lon, y = track$lat),
                  color = 'red', size = 1)

    print(site)
} else if (is.null(KEY)) {
    warning('Google API Key has not been obtained. Please see ?register_google
    and follow the details on how to obtain and enable an API Key.')
}


image2D(z, x = seq(min(track$lon), max(track$lon), length.out = nrow(z)),
                y = seq(min(track$lat), max(track$lat), length.out = ncol(z)),
                colvar = z, col = pal(4), NAcol = "white", breaks = NULL,
                border = NA, zlim = c(0, 2), facets = TRUE, contour = FALSE,
                colkey = ck, resfac = 1, clab = NULL, lighting = FALSE,
                shade = NA, ltheta = -135, lphi = 0, theta = 0,
                rasterImage = FALSE, add = FALSE, plot = TRUE,
                xlab = 'Longitude [dd]', ylab = "Latitude [dd]")

z <- data.frame(z)
return(list(z, bins, waypoint))

}
