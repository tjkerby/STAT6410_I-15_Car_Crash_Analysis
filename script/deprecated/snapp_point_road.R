
load("data-raw/RObject/flow_data.RData")
library(readr)
library(sf)
library(rgeos)
crash <- read.csv("data-raw/RawCrashData2020.csv")

crash_i15 <- crash %>%
  dplyr::select(., Route, Milepoint, Lat, Long) %>%
  dplyr::filter(Route == "0015")%>%
  arrange(Lat)

i15_spat <- sf::st_as_sf(
  x = crash_i15,
  coords = c("Long", "Lat"),
  crs = 4269
)

spdf1 <- as(i15_spat,  "Spatial")



sp_point = as(spdf1,"SpatialPoints")
crs(sp_point) <- "+proj=longlat +datum=WGS84 +units=m +no_defs"

load("data-raw/RObject/I_15_line.RData")
 l = as(I_15_line, "Spatial")
 
 sp_line <- as(l,"SpatialLines")
 crs( sp_line) <- "+proj=longlat +datum=WGS84 +units=m +no_defs"

 
 

(d <- rgeos::gProject(spgeom =  sp_line, sppoint =  sp_point))
