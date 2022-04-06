load("./data-raw/RObject/crashes.RData")
load("./data-raw/RObject/station_points.RData")

# Converts the distance to bottom to just a number (in meters still)
station_points$distance <- as.numeric(station_points$dist_to_bottom)

# Finds the index of the station which is closest to the crash
crashes$closest_station <- sapply(1:nrow(crashes), function(i) {
  which.min(abs(crashes$distance[i] - station_points$distance))[1]
})

# For each station, counts the number of crashes where this is the closests station
station_points$num_crashes <- sapply(1:nrow(station_points), function(i) {
  sum(crashes$closest_station == station_points$id[i])
})

# Normalizes the number of crashes by volume
station_points$normalized_crashes <- station_points$num_crashes / station_points$Flow

# Calculates the length that the station covers
station_points$dist_to_station <- as.numeric(station_points$dist_to_station)
station_points$range <- sapply(1:nrow(station_points), function(i) {
  if (i == 1) {
    station_points$dist_to_station[i] + (station_points$dist_to_station[i + 1] / 2)
  } else if (i == nrow(station_points)) {
    station_points$dist_to_station[i] # Assumes same length northward
  } else {
    (station_points$dist_to_station[i] / 2) + (station_points$dist_to_station[i + 1] / 2)
  }
})
station_points$distance <- NULL
station_points <- as.data.frame(station_points)
station_points$geometry <- NULL


save(station_points, file="./data-raw/RObject/station_points_final.RData")
