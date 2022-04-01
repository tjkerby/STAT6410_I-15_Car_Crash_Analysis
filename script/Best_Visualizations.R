library("dplyr")
library("tigris")
library("ggplot2")
library("sf")
library("ggmap")
library("ggeasy")
library("ggspatial")
library("viridis")
library("gridExtra")
library("gridtext")
library("grid")
library("cowplot")
################################################################################
load("data-raw/RObject/roads_i15")
load("data-raw/RObject/i15_spat")
load("data-raw/RObject/stat_spat")
load("data-raw/RObject/ut_map")
load("data-raw/RObject/samp_map")
load("data-raw/RObject/ut_map_3857")

load("data-raw/RObject/ut_map_3857")
load("data-raw/RObject/ut_outline_3857")

################################################################################
roads_3857 <- sf::st_transform(roads_i15, 3857)
crash_3857 <- sf::st_transform(i15_spat, 3857)
station_3857 <- sf::st_transform(stat_spat, 3857)
ut_outline_3857 <- sf::st_transform(ut_map, 3857)

# Terrain Plot of Utah,I15, Car Accidents, and stations
all_plot <- ggmap(terr_ut_3857) +
  geom_sf(
    data = roads_3857,
    inherit.aes = FALSE,
    fill = NA,
    lwd = 1.25,
    aes(color = "red"),
  ) +
  geom_sf(
    data = crash_3857,
    inherit.aes = FALSE,
    aes(color = "blue"),
    alpha = 0.05
  ) +
  geom_sf(
    data = station_3857,
    inherit.aes = FALSE,
    shape = 3,
    aes(color = "yellow"),
    lwd = 1,
  ) +
  geom_sf(
    data = ut_outline_3857,
    inherit.aes = FALSE,
    fill = NA
  )


fin_all_plot <- all_plot +
  scale_color_identity(
    name = "Legend",
    breaks = c("red", "blue", "yellow"),
    labels = c("I-15", "Car Accident", "Station"),
    guide = "legend"
  ) +
  theme(
    plot.title = element_text(size = 25, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 10, face = "bold.italic"),
    legend.text = element_text(size = 10, face = "bold.italic")
  ) +
  ggtitle(
    "I-15 Utah Car Accidents\n and Flow Stations"
  ) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggeasy::easy_center_title()

fin_all_plot
################################################################################
load("data-raw/RObject/I_15_line.RData")
load("data-raw/RObject/roads_i15")
################################################################################
tbbox <- sf::st_bbox(ut_map)
names(tbbox) <- c("left", "bottom", "right", "top")
samp_map <- ggmap::get_map(
  location = tbbox,
  maptype = "terrain",
  source = "stamen"
)

################################################################################
I_15_line_3857 <- sf::st_transform(I_15_line, 3857)
### Without Terrain Map and Utah Outline
I_15_line_road <- ggplot() +
  geom_sf(
    data = roads_3857,
    aes(color = "red"),
    lwd = 1
  ) +
  geom_sf(
    data = I_15_line_3857,
    aes(color = "blue"),
    linetype = "dashed",
    lwd = 1
  )

I_15_line_road_fin <- I_15_line_road +
  scale_color_identity(
    name = "Legend",
    breaks = c("red", "blue"),
    labels = c("I-15 Road", "Projected I-15 Line"),
    guide = "legend"
  ) +
  theme(
    plot.title = element_text(size = 25, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 10, face = "bold.italic"),
    legend.text = element_text(size = 10, face = "bold.italic")
  ) +
  ggtitle(
    " Actual I-15 vs Projected I-15"
  ) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggeasy::easy_center_title() +
  theme_void()

I_15_line_road_fin
### WIthout Terrain map but with Utah outline
I_15_line_road_o <- ggplot() +
  geom_sf(
    data = roads_3857,
    aes(color = "red"),
    lwd = 1
  ) +
  geom_sf(
    data = I_15_line_3857,
    aes(color = "blue"),
    linetype = "dashed",
    lwd = 1
  ) +
  geom_sf(
    data = ut_outline_3857,
    inherit.aes = FALSE,
    fill = NA
  )

I_15_line_road_o_fin <- I_15_line_road_o +
  theme_void() +
  scale_color_identity(
    name = "Legend",
    breaks = c("red", "blue"),
    labels = c("I-15 Road", "Projected I-15 Line"),
    guide = "legend"
  ) +
  theme(
    plot.title = element_text(size = 25, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 10, face = "bold.italic"),
    legend.text = element_text(size = 10, face = "bold.italic")
  ) +
  ggtitle(
    " Actual I-15 vs Projected I-15"
  ) +
  ggeasy::easy_center_title()

I_15_line_road_o_fin
### With Terrain Map
I_15_line_road_t <- ggmap(ut_map_3857) +
  geom_sf(
    data = roads_3857,
    aes(color = "red"),
    inherit.aes = FALSE,
    lwd = 1
  ) +
  geom_sf(
    data = I_15_line_3857,
    aes(color = "blue"),
    inherit.aes = FALSE,
    linetype = "dashed",
    lwd = 1
  ) +
  geom_sf(
    data = ut_outline_3857,
    inherit.aes = FALSE,
    fill = NA
  )

I_15_line_road_t_fin <- I_15_line_road_t +
  scale_color_identity(
    name = "Legend",
    breaks = c("red", "blue"),
    labels = c("I-15 Road", "Projected I-15 Line"),
    guide = "legend"
  ) +
  theme(
    plot.title = element_text(size = 25, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 10, face = "bold.italic"),
    legend.text = element_text(size = 10, face = "bold.italic")
  ) +
  ggtitle(
    "Actual I-15 vs Projected I-15"
  ) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggeasy::easy_center_title()

I_15_line_road_t_fin

################################################################################

I_15_line_sub <- sf::st_crop(I_15_line, xmin = -111.5, ymin = 39.2, xmax = -112.5, ymax = 40.2)
roads_i15_sub <- sf::st_crop(roads_i15, xmin = -111.5, ymin = 39.2, xmax = -112.5, ymax = 40.2)
stat_spat_sub <- sf::st_crop(stat_spat, xmin = -111.5, ymin = 39.2, xmax = -112.5, ymax = 40.2)

bbox_sub <- sf::st_bbox(roads_i15_sub)

### SUBSET BOUNDING BOX
I_15_line_road +
  ggspatial::layer_spatial(bbox_sub, fill = NA)
################################################################################
I_15_line_sub_3857 <- sf::st_transform(I_15_line_sub, 3857)
roads_i15_sub_3857 <- sf::st_transform(roads_i15_sub, 3857)
stat_spat_sub_3857 <- sf::st_transform(stat_spat_sub, 3857)

I_15_line_road_sub <- ggplot() +
  geom_sf(
    data = I_15_line_sub_3857,
    aes(color = "red"),
    lwd = 1
  ) +
  geom_sf(
    data = roads_i15_sub_3857,
    aes(color = "blue"),
    lwd = 1
  ) +
  geom_sf(
    data = stat_spat_sub_3857,
    aes(color = "black")
  )

I_15_line_road_sub_fin <- I_15_line_road_sub +
  theme_void() +
  scale_color_identity(
    name = "Legend",
    breaks = c("red", "blue", "black"),
    labels = c("I-15 Road", "Projected I-15 Line", "Stations"),
    guide = "legend"
  ) +
  theme(
    plot.title = element_text(size = 25, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 10, face = "bold.italic"),
    legend.text = element_text(size = 10, face = "bold.italic")
  ) +
  ggtitle(
    " Actual I-15 vs Projected I-15"
  ) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggeasy::easy_center_title()
I_15_line_road_sub_fin
################################################################################
load("./data-raw/RObject/station_points_final.RData")

station_points_sub <- sf::st_crop(station_points, xmin = -111.5, ymin = 40, xmax = -112, ymax = 41)
# 263 to 5783
#,plot.margin=unit(c(1,10,1,0),"cm")
#,plot.margin = unit(c(1,0,1,0),"cm")
#,plot.margin=unit(c(1,0,1,-15),"cm")
flow_plot <- ggplot() +
  geom_sf(station_points,
    mapping = aes(color = Flow/100)
  ) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.box="horizontal",
        plot.margin = unit(c(1,-15,1,0),"cm"),
                           plot.title = element_text(size = 15, face = "bold.italic"),
                           legend.title = element_text(size = 10, face = "bold.italic"),
                           legend.text = element_text(size = 10, face = "bold.italic")
        ) +
  ggtitle(
    "Flow"
  ) +
  ggeasy::easy_center_title() +
  guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5),
         size = guide_legend(title.position="top", title.hjust = 0.5)
         ) +
  scale_color_viridis(option = "magma")

# number of car crashes
num_crashes_plot <- ggplot() +
  geom_sf(station_points,
    mapping = aes(color = num_crashes)
  ) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.box="horizontal",
        plot.margin = unit(c(1,0,1,0),"cm"),
        plot.title = element_text(size = 15, face = "bold.italic"),
        legend.title = element_text(size = 10, face = "bold.italic"),
        legend.text = element_text(size = 10, face = "bold.italic")
  ) +
  ggtitle(
    "Crashes"
  ) +
  ggeasy::easy_center_title() +
  guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5),
         size = guide_legend(title.position="top", title.hjust = 0.5)
  ) +
  scale_color_viridis(option = "magma")

# normalized plot
normalized_crashes_plot <- ggplot() +
  geom_sf(station_points,
    mapping = aes(color = normalized_crashes)
  ) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.box="horizontal",
        plot.margin = unit(c(1,0,1,-15),"cm"),
        plot.title = element_text(size = 15, face = "bold.italic"),
        legend.title = element_text(size = 10, face = "bold.italic"),
        legend.text = element_text(size = 10, face = "bold.italic")
  ) +
  ggtitle(
    "Crashes Over Flow"
  ) +
  ggeasy::easy_center_title() +
  guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5),
         size = guide_legend(title.position="top", title.hjust = 0.5)
  ) +
  scale_color_viridis(option = "magma")

plot_list <- list(flow_plot,
                  num_crashes_plot,
                  normalized_crashes_plot
                  )
grid.arrange(
  grobs = plot_list,
  nrow = 1
)
