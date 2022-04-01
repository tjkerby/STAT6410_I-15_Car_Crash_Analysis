load("data-raw/RObject/projected_acc.RData")
load("data-raw/RObject/station_points.RData")

get_dist <- function(crashes, stations) {
  dist <- c()
  segment_distances <- c()
  lat <- array(stations$Latitude)
  for (i in 1:nrow(crashes)) {
    station_below <- station_points[station_points$Latitude == max(lat[lat <= st_coordinates(crashes[i,])[1]]),][1,]
    new_dist <- as.numeric(sf::st_distance(crashes[i,], station_below))
    if (is.na(new_dist)) {
      new_dist <- 0
    }
    dist <- c(dist, new_dist + as.numeric(station_below$dist_to_bottom))
    segment_distances <- c(segment_distances, new_dist)
  }
  list("dist" = dist, "segment_dista" = segment_distances)
}


station_points$Latitude <- st_coordinates(station_points$geometry)[,1]
crashes <- projected_acc
distances <- get_dist(projected_acc, station_points)
crashes$distance <- distances[["dist"]]
crashes$dist_to_station <- distances[["segment_dista"]]
crashes <- crashes[!is.na(crashes$distance), ]

library(ggplot2)
par(mfrow = c(1,2))
ggplot(crashes) + geom_sf(aes(col = distance))
ggplot(crashes) + geom_sf(aes(col = dist_to_station))
ggplot() + geom_point(aes(x=Longitude, y=Latitude, col = distance), data = crashes)

save(crashes, file = "data-raw/RObject/crashes.RData")



