library(ggplot2)
library(dplyr)
load("./data-raw/RObject/station_points_final.RData")

# Estimated
crash_density <- ksmooth(station_points$dist_to_bottom,
                         station_points$num_crashes/sum(station_points$num_crashes),
                         "normal",
                         40000,)

# expected based on flow
station_points$expected <- sum(station_points$num_crashes)*
  (station_points$Flow/sum(station_points$Flow))
station_points$expected_norm <- station_points$expected/station_points$Flow

plot(station_points$dist_to_bottom,
     station_points$num_crashes)
points(station_points$dist_to_bottom,
       station_points$expected, type = "p", col="red")
lines(crash_density)


# crashes by flow, statndardized by distance
station_points$expected_1 <- (station_points$num_crashes/station_points$Flow) *
  (station_points$range / sum(station_points$range))

station_points$normalized_crashes_1 <- station_points$normalized_crashes / sum(station_points$normalized_crashes)
station_points$expected_1 <- station_points$expected_1 / sum(station_points$expected_1)

plot(station_points$dist_to_bottom,
     station_points$normalized_crashes_1,
     ylim=c(0,.4))
points(station_points$dist_to_bottom,
       station_points$expected_1, col="red",
       ylim=c(0,.4))


# Expected number of crashes by flow
total_crashes <- sum(station_points$num_crashes)
prop_flow <- station_points$Flow / sum(station_points$Flow)
station_points$num_crashes_exp_flow <- total_crashes * prop_flow
station_points$num_crashes_exp_flow <- station_points$num_crashes_exp_flow/sum(station_points$num_crashes_exp_flow)

# expected number of crashes by distance
prop_dist <- station_points$range / sum(station_points$range)
station_points$num_crashes_exp_dist <- total_crashes * prop_dist
station_points$num_crashes_exp_dist <- station_points$num_crashes_exp_dist/sum(station_points$num_crashes_exp_dist)

# expected number of crashes by flow and distance
station_points$num_crashes_exp_both <- station_points$num_crashes / mean(station_points$Flow)
station_points$num_crashes_exp_both <- station_points$num_crashes_exp_both/sum(station_points$num_crashes_exp_both)

ylim <- max(c(station_points$num_crashes/sum(station_points$num_crashes),
              station_points$num_crashes_exp_both,
              station_points$num_crashes_exp_dist,
              station_points$num_crashes_exp_flow))

plot(station_points$dist_to_bottom,
     station_points$num_crashes/sum(station_points$num_crashes),
     ylim = c(0, ylim))
points(station_points$dist_to_bottom,
       station_points$num_crashes_exp_flow,
       col = "red",
       ylim = c(0, ylim))
points(station_points$dist_to_bottom,
       station_points$num_crashes_exp_dist,
       col = "blue",
       pch = 20,
       ylim = c(0, ylim))
# points(station_points$dist_to_bottom,
#        station_points$num_crashes_exp_both,
#        col = "Green",
#        pch = 10,
#        ylim = c(0, ylim))
lines(crash_density)


#############
# Start chi square tests
chisq.test(x = station_points$num_crashes,
           p = station_points$num_crashes_exp_flow)
chisq.test(x = station_points$num_crashes,
           p = station_points$num_crashes_exp_dist)
chisq.test(x = round(station_points$num_crashes/station_points$Flow * 20) + 1,
           p = station_points$num_crashes_exp_both)


#################
# Generalizes the expected counts function

expected_count <- function(data, alpha=.5,
                           var1 = "Flow",
                           var2 = "range",
                           counts = "num_crashes") {
  if (!(0 <= alpha && alpha <= 1)) {
    stop("alpha needs to be between 0 and 1")
  }
  
  total_counts <- sum(data[,counts])
  prop_var1 <- as.vector(data[,var1]) / sum(data[,var1])
  prop_var2 <- as.vector(data[,var2]) / sum(data[,var2])
  new_prop <- alpha * prop_var1 + (1 - alpha) * prop_var2
  as.vector(total_counts * new_prop)
}

station_points$exp_flow <- expected_count(station_points,
                                          alpha = 1)$Flow
station_points$exp_range <- expected_count(station_points,
                                           alpha = 0)$Flow
station_points$exp_both <- expected_count(station_points,
                                          alpha = .5)$Flow
station_points$exp_maj_flow <- expected_count(station_points,
                                              alpha = .75)$Flow

plotting_points <- station_points %>%
  dplyr::select(num_crashes, dist_to_bottom,
                exp_flow, exp_range,
                exp_both, exp_maj_flow) %>%
  tidyr::pivot_longer(cols = c(num_crashes,
                               exp_flow,
                               exp_range,
                               exp_both,
                               exp_maj_flow))

ggplot(data = plotting_points, aes(x = dist_to_bottom,
                                  y = value,
                                  color = name)) +
  geom_point()
  


# Start chi square tests
chisq.test(x = station_points$num_crashes,
           p = station_points$exp_flow/sum(station_points$exp_flow))
chisq.test(x = station_points$num_crashes,
           p = station_points$exp_range/sum(station_points$exp_range))
chisq.test(x = station_points$num_crashes,
           p = station_points$exp_both/sum(station_points$exp_both))
chisq.test(x = station_points$num_crashes,
           p = station_points$exp_maj_flow/sum(station_points$exp_maj_flow))
