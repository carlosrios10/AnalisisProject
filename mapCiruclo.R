data = data.frame(
    ID = as.numeric(c(1:8)),
    longitude = as.numeric(c(-63.27462, -63.26499, -63.25658, -63.2519, -63.2311, -63.2175, -63.23623, -63.25958)),
    latitude = as.numeric(c(17.6328, 17.64614, 17.64755, 17.64632, 17.64888, 17.63113, 17.61252, 17.62463))
)

#################################################################################
# create circles data frame from the centers data frame
make_circles <- function(centers, radius, nPoints = 100){
    # centers: the data frame of centers with ID
    # radius: radius measured in kilometer
    #
    meanLat <- mean(centers$latitude)
    # length per longitude changes with lattitude, so need correction
    radiusLon <- radius /111 / cos(meanLat/57.3) 
    radiusLat <- radius / 111
    circleDF <- data.frame(ID = rep(centers$ID, each = nPoints))
    angle <- seq(0,2*pi,length.out = nPoints)
    
    circleDF$lon <- unlist(lapply(centers$longitude, function(x) x + radiusLon * cos(angle)))
    circleDF$lat <- unlist(lapply(centers$latitude, function(x) x + radiusLat * sin(angle)))
    return(circleDF)
}

# here is the data frame for all circles
myCircles <- make_circles(data, 0.45)
##################################################################################


island = get_map(location = c(lon = -63.247593, lat = 17.631598), zoom = 13, maptype = "satellite")
islandMap = ggmap(island, extent = "panel", legend = "bottomright")
RL = geom_point(aes(x = longitude, y = latitude), data = data, color = "#ff0000")
islandMap + RL + 
    scale_x_continuous(limits = c(-63.280, -63.21), expand = c(0, 0)) + 
    scale_y_continuous(limits = c(17.605, 17.66), expand = c(0, 0)) +
    ########### add circles
    geom_polygon(data = myCircles, aes(lon, lat, group = ID), color = "red", alpha = 0)

islandMap + RL + 
    scale_x_continuous(limits = c(-63.280, -63.21), expand = c(0, 0)) + 
    scale_y_continuous(limits = c(17.605, 17.66), expand = c(0, 0)) + 
    geom_point(aes(x = longitude, y = latitude), data = data, size = 20, shape = 1,  color = "#ff0000")
