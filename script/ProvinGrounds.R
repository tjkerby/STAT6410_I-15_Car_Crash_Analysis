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


roads <- tigris::primary_secondary_roads("Utah") %>%
  dplyr::filter(RTTYP %in% c("I"))
roads_i15 <- roads[which(roads$FULLNAME == "I- 15"), ] %>%
  sf::st_cast(., "MULTILINESTRING")

# my_id_touches <- st_touches(roads_i15)
my_id_touches <-  st_is_within_distance(roads_i15, dist=15)
my_igraph <- graph_from_adj_list(my_id_touches)
my_comps <- components(my_igraph)$membership

new_i15 <- roads_i15 %>%
  group_by(section = as.character({{my_comps}})) %>%
  summarise()

road_plot <- ggplot() +
  geom_sf(
    data = new_i15,
    color = "blue",
    aes(geometry = geometry)
  )
road_plot


road_plot <- ggplot() +
  geom_sf(
    data = roads_i15,
    color = "blue",
    aes(geometry = geometry)
  )
road_plot

i15_poly <- st_cast(new_i15, "POLYGON")

road_plot <- ggplot() +
  geom_sf(
    data = i15_poly,
    color = "blue",
    aes(geometry = geometry)
  )
road_plot
###############################

library(concaveman)
i15_poly <- concaveman(new_i15) %>%
  st_zm()
plot(i15_poly)
