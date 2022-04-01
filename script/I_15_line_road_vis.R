library(ggplot2)
load("data-raw/RObject/I_15_line.RData")
load("data-raw/RObject/roads_i15")
load("data-raw/RObject/stat_spat")

I_15_line
roads_i15
stat_spat
bbox_I_15 <- sf::st_bbox(xmin = -111.5,ymin = 39, xmax = -112.5,ymax = 41)

ggplot() +
  geom_sf(
    data = roads_i15,
    color = "red"
  ) +
  geom_sf(
    data = I_15_line,
    color = "blue",
  )

I_15_line_sub <- sf::st_crop(I_15_line,xmin = -111.5,ymin = 39.2, xmax = -112.5,ymax = 40.2)
roads_i15_sub <- sf::st_crop(roads_i15,xmin = -111.5,ymin = 39.2, xmax = -112.5,ymax = 40.2) 
stat_spat_sub <- sf::st_crop(stat_spat,xmin = -111.5,ymin = 39.2, xmax = -112.5,ymax = 40.2)

ggplot()+
  geom_sf(
    data = roads_i15_sub,
    color = "red"
  ) +
  geom_sf(
    data = I_15_line_sub,
    color = "blue",
  )+
  geom_sf(
    data = stat_spat_sub,
    color = "green1"
  ) 
