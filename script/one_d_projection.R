library(sf)
library(dplyr)

load("data-raw/RObject/flow_data.RData")
crash <- read.csv("data-raw/RawCrashData2020.csv")

bottom_crash <- st_as_sf(crash[crash$Lat == min(crash$Lat), ],
         coords = c("Long", "Lat"), crs = 4269)

plot(flow_data$Longitude, flow_data$Latitude)

get_dist <- function(bottom_crash, flow_data) {
  lat_data <- flow_data[order(flow_data$Latitude), ]
  lat_data$id <- seq(1:nrow(lat_data))
  lat_data <- st_as_sf(lat_data,
           coords = c("Longitude", "Latitude"),
           crs = 4269)
  dist <- c()
  dist_station_south <- c()
  i = 1
  for (i in 1:nrow(lat_data)) {
    if(i == 1) {
      new_dist <- sf::st_distance(bottom_crash, lat_data[i,])
      dist <- c(new_dist)
      dist_station_south <- c(new_dist)
    } else {
      new_dist <- sf::st_distance(lat_data[i-1,], lat_data[i,])
      dist <- c(dist, new_dist + dist[i - 1])
      dist_station_south <- c(dist_station_south, new_dist)
    }
  }
  list("dist" = dist, "dist_to_station" = dist_station_south)
}

flow_data <- flow_data[order(flow_data$Latitude), ]
results <- get_dist(bottom_crash, flow_data)
flow_data$dist_to_bottom <- results[["dist"]]
flow_data$dist_to_station <- results[["dist_to_station"]]
flow_data$id <- seq(1:nrow(flow_data))

station_points <- st_as_sf(flow_data,
                 coords = c("Longitude", "Latitude"),
                 crs = 4269)
station_points <- station_points %>% arrange(id)

I_15_line <- station_points %>% summarise(do_union = FALSE) %>% st_cast("LINESTRING")
plot(I_15_line)

save(station_points, file = "data-raw/RObject/station_points.RData")
save(I_15_line, file = "data-raw/RObject/I_15_line.RData")
