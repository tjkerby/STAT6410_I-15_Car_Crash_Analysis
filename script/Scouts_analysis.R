library(ggplot2)
library(ggthemes)
library(ggeasy)
library(dplyr)
library(viridis)
library(RColorBrewer)
load("./data-raw/RObject/station_points_final.RData")

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
  as.vector(total_counts * new_prop)$Flow
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


pdf("./figures/actualVSexpectedCrashes.pdf", width = 9, height = 5)
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
    plot.title = element_text(size = 25, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 10, face = "bold.italic"),
    legend.text = element_text(size = 10, face = "bold.italic")
  ) +
  labs(title = "Actual VS Expected Number of Crashes",
       y = "Number of Crashes",
       x = "Distance Along I-15 (m)",
       size = "Flow",
       color = "Crash Type") +
  easy_center_title()
dev.off()


# Plots range by crashes normalized by 
ggplot(station_points,
       aes(x = log(range),
           y = normalized_crashes,
           color = TRUE)) +
  geom_point() +
  scale_color_manual(values = c(colors[6]),
                                 labels = FALSE) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 25, face = "bold.italic"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 10, face = "bold.italic"),
    legend.text = element_text(size = 10, face = "bold.italic")
  ) +
  labs(title = "Crashes/Flow by Range",
       y = "Crashes / Flow",
       x = "Range of Station (m)",
       size = FALSE,
       color = FALSE) +
  legend(position = NULL)
  easy_center_title()


# Start chi square tests
chisq.test(
  x = station_points$num_crashes,
  p = station_points$exp_flow / sum(station_points$exp_flow)
)
chisq.test(
  x = station_points$num_crashes,
  p = station_points$exp_range / sum(station_points$exp_range)
)
chisq.test(
  x = station_points$num_crashes,
  p = station_points$exp_both / sum(station_points$exp_both)
)
chisq.test(
  x = station_points$num_crashes,
  p = station_points$exp_maj_flow / sum(station_points$exp_maj_flow)
)
