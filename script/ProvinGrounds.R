# For Data Cleaning and Visualization
library("dplyr")# For Data Cleaning and Visualization
library("dplyr")
library("tigris")
library("ggplot2")
library("sf")
# For AOI and climateR
library("lattice")
library("raster")
library("rasterVis")
library("AOI")
library("climateR")
library(igraph)
library(measurements)


# Turns the I15 multiline strings into a single multlinestring
roads <- tigris::primary_secondary_roads("Utah") %>%
  dplyr::filter(RTTYP %in% c("I"))
# roads_i15 <- roads[which(roads$FULLNAME == "I- 15"), ]

roads_i15 <- roads[which(roads$FULLNAME == "I- 15"), ] %>%
  sf::st_cast(., "MULTILINESTRING")

# my_id_touches <- st_touches(roads_i15)
my_id_touches <-  st_is_within_distance(roads_i15, dist=15)
my_igraph <- graph_from_adj_list(my_id_touches)
my_comps <- components(my_igraph)$membership

new_i15 <- roads_i15 %>%
  group_by(section = as.character({{my_comps}})) %>%
  summarise()
new_i15$col <- 1:nrow(new_i15)


all_plot <- ggplot(ut_map) +
  geom_sf(fill = NA) +
  geom_sf(aes(color = col),
    data = new_i15
  )
all_plot


rm(my_id_touches, my_igraph, roads, roads_i15, my_comps)


################################

# Finds the point on I15 where the crash is closest

i15_points <- st_cast(new_i15, "POINT")
crash <- read.csv("./data-raw/RawCrashData2020.csv")
crash_i15 <- crash %>%
  dplyr::select(., Route, Milepoint, Lat, Long) %>%
  dplyr::filter(Route == "0015")
i15_spat <- sf::st_as_sf(
  x = crash_i15,
  coords = c("Long", "Lat"),
  crs = 4269
)

rm(crash, crash_i15)
# Finds the point on i15 closest to crash

# distances <- st_distance(i15_spat, i15_points)
# save(distances, file="./data-raw/crashPointDist.rdata")
load("./data-raw/crashPointDist.rdata")
mins <- apply(distances, 1, which.min)
i15_spat$geometry <- i15_points[mins,]$geometry


i15_points <- i15_points %>%
  mutate(lat = st_coordinates(geometry)[,2]) %>%
  arrange(lat)
dists <- sapply(2:nrow(i15_points), function(i) {
  dist <- st_distance(i15_points[i,], i15_points[i-1,])
})
i15_points$dist <- c(0, cumsum(dists))
i15_points$lat <- NULL
i15_points$section <- NULL

i15_spat <- st_join(i15_spat, i15_points)
i15_spat$dist <-  conv_unit(i15_spat$dist, "m", "mi")


# find distance from southern most point to all points
south_point <- i15_points[which.min(st_coordinates(i15_points)[,2])[1],]
# north_point <- i15_points[which.max(st_coordinates(i15_points)[,2])[1],]
# This gives euclidean distance from south point to all points.
point_distances <- st_distance(i15_spat, south_point)
i15_spat$dist <- point_distances
i15_spat$dist <-  conv_unit(i15_spat$dist, "m", "mi")
