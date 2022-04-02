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
