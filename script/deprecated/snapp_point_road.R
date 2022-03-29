
load("data-raw/RObject/flow_data.RData")
library(readr)
library(sf)
library(rgeos)
crash <- read.csv("../data-raw/RawCrashData2020.csv")

crash_i15 <- crash %>%
  dplyr::select(., Route, Milepoint, Lat, Long) %>%
  dplyr::filter(Route == "0015")

i15_spat <- sf::st_as_sf(
  x = crash_i15,
  coords = c("Long", "Lat"),
  crs = 4269
)

i15_spat <- i15_spat[150,]
spdf1 <- as(i15_spat,  "Spatial")


i <- roads_i15[11, ]
i <- sf::st_transform(i, crs = 4269)
spdf2 <- as(i, "Spatial")


d <- rgeos::gProject(spgeom = spdf2, sppoint = spdf1)

dataset <- cbind(rawcrash, d)

splns2 <- st_combine(roads_i15)
splns3 <- st_line_merge(splns2)


stc <- st_cast(roads_i15, "POLYGON")
plot(stc)

t <- as_Spatial(splns2)
t <- lapply(1:length(t), function(x) t[x, ])
splitLines(t[[1]], dist = 100)

on_line_all <- st_cast(splns2, "POINT")
buf_all <- st_combine(st_buffer(on_line_all, 0.1))
parts_all <- st_collection_extract(lwgeom::st_split(roads_i15$geometry, buf_all), "LINESTRING")

parts_all <- st_as_sf(
  data.frame(
    id = 1:length(parts_all),
    geometry = parts_all
  )
)

mapview(parts_all, burst = "id")
stc <- st_cast(parts_all, "POLYGON")
plot(stc)

sfff <- st_as_sf(on_line_all)
