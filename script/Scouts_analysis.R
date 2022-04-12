library(ggplot2)
library(ggthemes)
library(ggeasy)
library(dplyr)
library(viridis)
library(RColorBrewer)
library(ggformula)
load("./data-raw/RObject/station_points_final.RData")

station_points <- dplyr::arrange(station_points, dist_to_bottom)


#################
# Generalizes the expected counts function

expected_count <- function(data, alpha = .5,
                           var1 = "Flow",
                           var2 = "range",
                           counts = "num_crashes") {
  if (!(0 <= alpha && alpha <= 1)) {
    stop("alpha needs to be between 0 and 1")
  }

  total_counts <- sum(data[, counts])
  prop_var1 <- as.vector(data[, var1]) / sum(data[, var1])
  prop_var2 <- as.vector(data[, var2]) / sum(data[, var2])
  new_prop <- alpha * prop_var1 + (1 - alpha) * prop_var2
  total_counts * new_prop
}

# gets the expected number of crashes by flow
station_points$exp_flow <- expected_count(
  station_points,
  alpha = 1
)


# formats data better for plotting
plotting_points <- station_points %>%
  dplyr::select(
    num_crashes, dist_to_bottom,
    exp_flow, Flow
  ) %>%
  tidyr::pivot_longer(cols = c(
    num_crashes,
    exp_flow
  ))

# points for notable cities
cities_x <- c(8, 59, 300, 339, 253, 167)
cities_x <- cities_x * 1609.34
cities_name <- c("St. George", "Cedar City", "SLC", "Ogden", "Spanish Fork", "Nephi")

colors <- brewer.pal(8, "Dark2")

pdf("./figures/actualVSexpectedCrashes.pdf", width = 18, height = 10)
# Makes the plot of actual vs expected number of crashes
ggplot(data = plotting_points, aes(
  x = dist_to_bottom,
  y = value,
  color = name,
  size = Flow,
  # shape = name
)) +
  scale_color_manual(values = c(colors[5], colors[6]),
                     labels = c("Expected", "Actual")) +
  geom_point() +
  theme_bw() +
  theme(
    plot.title = element_text(size = 35, face = "bold.italic"),
    axis.title.x = element_text(size = 25, face = "bold"),
    axis.title.y = element_text(size = 25, face = "bold"),
    legend.title = element_text(size = 25, face = "bold.italic"),
    legend.text = element_text(size = 25, face = "bold.italic"),
    axis.text = element_text(size = 20)
  ) +
  labs(title = "Actual VS Expected Number of Crashes",
       y = "Number of Crashes",
       x = "Distance Along I-15 (m)",
       size = "Flow",
       color = "Type") +
  easy_center_title() +
  geom_vline(xintercept = cities_x,
             alpha = .7,
             linetype = "dotted") +
  annotate("text",
           x = cities_x,
           y = rep(275, length(cities_x)),
           label = cities_name,
           size = 7) +
  guides(color = guide_legend(override.aes = list(size = 5)))
dev.off()



# Plots range by crashes normalized by flow
pdf("./figures/norm_crashes_by_range.pdf", width = 18, height = 10)
ggplot(station_points,
       aes(x = log(range),
           y = normalized_crashes)) +
  geom_point(size = 4) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 35, face = "bold.italic"),
    axis.title.x = element_text(size = 24, face = "bold"),
    axis.title.y = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 20, face = "bold.italic"),
    legend.text = element_text(size = 20, face = "bold.italic"),
    legend.position = "none",
    axis.text = element_text(size = 20)
  ) +
  labs(title = "Crashes/Flow by Range",
       y = "Crashes / Flow",
       x = "Log(Range)",
       size = FALSE,
       color = FALSE) +
  easy_center_title()
dev.off()

# Start chi square tests
chisq.test(
  x = station_points$num_crashes,
  p = station_points$exp_flow / sum(station_points$exp_flow)
)


# Just look at the wasatch front area
sub_points <- station_points[25:(nrow(station_points)-5),]
sub_points$exp_flow <- expected_count(sub_points, 1)

# formats data better for plotting
plotting_points <- sub_points %>%
  dplyr::select(
    num_crashes, dist_to_bottom,
    exp_flow, Flow
  ) %>%
  tidyr::pivot_longer(cols = c(
    num_crashes,
    exp_flow
  ))

# points for notable cities
cities_x <- c(300, 339, 253, 284, 332)
cities_x <- cities_x * 1609.34
cities_name <- c("SLC", "Ogden", "Spanish Fork", "Lehi", "Layton")

colors <- brewer.pal(8, "Dark2")

pdf("./figures/sub_plot_actual_expected.pdf", width = 18, height = 10)
# Makes the plot of actual vs expected number of crashes
ggplot(data = plotting_points, aes(
  x = dist_to_bottom,
  y = value,
  color = name,
  size = Flow
)) +
  scale_color_manual(values = c(colors[5], colors[6]),
                     labels = c("Expected", "Actual")) +
  geom_point() +
  theme_bw() +
  theme(
    plot.title = element_text(size = 35, face = "bold.italic"),
    axis.title.x = element_text(size = 25, face = "bold"),
    axis.title.y = element_text(size = 25, face = "bold"),
    legend.title = element_text(size = 25, face = "bold.italic"),
    legend.text = element_text(size = 25, face = "bold.italic"),
    axis.text = element_text(size = 20)
  ) +
  labs(title = "Actual VS Expected Number of Crashes",
       y = "Number of Crashes",
       x = "Distance Along I-15 (m)",
       size = "Flow",
       color = "Type") +
  easy_center_title() +
  geom_vline(xintercept = cities_x,
             alpha = .7,
             linetype = "dotted") +
  annotate("text",
           x = cities_x,
           y = rep(275, length(cities_x)),
           label = cities_name,
           size = 7) +
  guides(color = guide_legend(override.aes = list(size = 5)))
dev.off()





# Chi-square test
chisq.test(
  x = sub_points$num_crashes,
  p = sub_points$exp_flow / sum(sub_points$exp_flow)
)




# smooth the flow
smooth_flow <- smooth.spline(sub_points$dist_to_bottom,
                             sub_points$Flow,
                             df = 20)

# Plots the smoothing spline
pdf("./figures/flow_smoothing_spline.pdf", width = 18, height = 10)
ggplot(data = sub_points, aes(x = dist_to_bottom, y = Flow)) +
  geom_point(size = 4) +
  geom_spline(aes(dist_to_bottom, Flow),
              data = sub_points,
              df = 20,
              color = "red",
              size = 2) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 35, face = "bold.italic"),
    axis.title.x = element_text(size = 24, face = "bold"),
    axis.title.y = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 20, face = "bold.italic"),
    legend.text = element_text(size = 20, face = "bold.italic"),
    legend.position = "none",
    axis.text = element_text(size = 20)
  ) +
  labs(title = "Flow Smoothing Spline",
       y = "Flow",
       x = "Distance Along I-15 (m)",
       size = FALSE,
       color = FALSE) +
  easy_center_title() +
  geom_vline(xintercept = cities_x,
             alpha = .7,
             linetype = "dotted") +
  annotate("text",
           x = cities_x,
           y = rep(6000, length(cities_x)),
           label = cities_name,
           size = 6.5)
dev.off()


# Gets smoothed flow
sub_points$smooth_flow <- predict(smooth_flow,
                                  sub_points$dist_to_bottom)$y
sub_points$exp_flow_s <- expected_count(sub_points,
                                        1,
                                        "smooth_flow")

# Plots new flow expected counts
# Makes the plot of actual vs expected number of crashes
plotting_points <- sub_points %>%
  dplyr::select(
    num_crashes, dist_to_bottom,
    exp_flow_s, smooth_flow
  ) %>%
  tidyr::pivot_longer(cols = c(
    num_crashes,
    exp_flow_s
  ))

pdf("./figures/sub_region_smoothed_flow.pdf", width = 18, height = 10)
ggplot(data = plotting_points, aes(
  x = dist_to_bottom,
  y = value,
  color = name,
  size = smooth_flow
)) +
  scale_color_manual(values = c(colors[5], colors[6]),
                     labels = c("Expected", "Actual")) +
  geom_point() +
  theme_bw() +
  theme(
    plot.title = element_text(size = 35, face = "bold.italic"),
    axis.title.x = element_text(size = 25, face = "bold"),
    axis.title.y = element_text(size = 25, face = "bold"),
    legend.title = element_text(size = 25, face = "bold.italic"),
    legend.text = element_text(size = 25, face = "bold.italic"),
    axis.text = element_text(size = 20)
  ) +
  labs(title = "Actual VS Expected Number of Crashes",
       y = "Number of Crashes",
       x = "Distance Along I-15 (m)",
       size = "Flow",
       color = "Type") +
  easy_center_title() +
  geom_vline(xintercept = cities_x,
             alpha = .7,
             linetype = "dotted") +
  annotate("text",
           x = cities_x,
           y = rep(275, length(cities_x)),
           label = cities_name,
           size = 7) +
  guides(color = guide_legend(override.aes = list(size = 5)))
dev.off()

# Chi-square test
chisq.test(
  x = sub_points$num_crashes,
  p = sub_points$exp_flow_s / sum(sub_points$exp_flow_s)
)




# Plotting work zone
colors <- brewer.pal(8, "Dark2")

work_lehi <- c(279, 284) * 1609.34
work_layton <- c(330, 339) * 1609.34
work_slc <- c(295, 297) * 1609.34

# points for notable cities
cities_x <- c(300, 339, 253, 284, 332)
cities_x <- cities_x * 1609.34
cities_name <- c("SLC", "Ogden", "Spanish Fork", "Lehi", "Layton")


pdf("./figures/road_work.pdf", width = 18, height = 10)
# Makes the plot of actual vs expected number of crashes
ggplot(data = plotting_points, aes(
  x = dist_to_bottom,
  y = value,
  color = name,
  size = Flow
)) +
  scale_color_manual(values = c(colors[5], colors[6]),
                     labels = c("Expected", "Actual")) +
  annotate("rect",
           xmin = work_layton[1],
           xmax = work_layton[2],
           ymin = -Inf,
           ymax = Inf,
           alpha = .25,
           color = NA,
           fill = "red")  +
  annotate("rect",
           xmin = work_lehi[1],
           xmax = work_lehi[2],
           ymin = -Inf,
           ymax = Inf,
           alpha = .25,
           color = NA,
           fill = "red")+
  annotate("rect",
           xmin = work_slc[1],
           xmax = work_slc[2],
           ymin = -Inf,
           ymax = Inf,
           alpha = .25,
           color = NA,
           fill = "red")+
  geom_vline(xintercept = cities_x,
             alpha = .7,
             linetype = "dotted") +
  annotate("text",
           x = cities_x,
           y = rep(275, length(cities_x)),
           label = cities_name,
           size = 7) +
  geom_point() +
  theme_bw() +
  theme(
    plot.title = element_text(size = 35, face = "bold.italic"),
    axis.title.x = element_text(size = 25, face = "bold"),
    axis.title.y = element_text(size = 25, face = "bold"),
    legend.title = element_text(size = 25, face = "bold.italic"),
    legend.text = element_text(size = 25, face = "bold.italic"),
    axis.text = element_text(size = 20)
  ) +
  labs(title = "Areas of Construction",
       y = "Number of Crashes",
       x = "Distance Along I-15 (m)",
       size = "Flow",
       color = "Type") +
  easy_center_title() +
  guides(color = guide_legend(override.aes = list(size = 5)))
dev.off()