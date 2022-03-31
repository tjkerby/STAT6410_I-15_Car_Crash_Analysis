library(sf)
library(dplyr)

load("data-raw/RObject/flow_data.RData")
crash <- read.csv("data-raw/RawCrashData2020.csv")

plot(flow_data$Longitude, flow_data$Latitude)


get_dist <- function(flow_data) {
  lat_data <- flow_data[order(flow_data$Latitude), ]
  dist <- c(0)
  dist_station_south <- c(0)
  for(i in 2:length(lat_data$Latitude)) {
    new_dist <- sqrt((lat_data$Longitude[i] - lat_data$Longitude[i - 1])^2 + 
      (lat_data$Latitude[i] - lat_data$Latitude[i - 1])^2) # Euclidean distance
    dist <- c(dist, new_dist + dist[i - 1])
    dist_station_south <- c(dist_station_south, new_dist)
  }
  list("dist" = dist, "dist_to_station" = dist_station_south)
}

flow_data <- flow_data[order(flow_data$Latitude), ]
results <- get_dist(flow_data)
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
