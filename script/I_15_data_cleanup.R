library(tidyverse)
library(data.table)

# Read in data and drop unnecessary columns
post_to_latlon <- read.csv("data-raw/Postmile_to_latlon.csv")
flow_n <- read.csv("data-raw/flow_2020_N_I_15.csv")
flow_s <- read.csv("data-raw/flow_2020_S_I_15.csv")

flow_s <- subset(flow_s,
  select = -c(Freeway, Direction, Minimum, Maximum, X..Lane.Points)
)
flow_n <- subset(flow_n,
  select = -c(Freeway, Direction, Minimum, Maximum, X..Lane.Points)
)



# Remove bad quality stations
flow_n <- flow_n[flow_n$X..Observed > 75, ]
flow_s <- flow_s[flow_s$X..Observed > 75, ]



# Remove duplicate stations whose only difference are slight changes to Postmile Abs.
flow_n <- flow_n[!duplicated(flow_n[c("Station")]), ]
flow_s <- flow_s[!duplicated(flow_s[c("Station")]), ]



# Remove Commas that mess up converting to a numeric for the mean.
flow_s$Mean <- as.numeric(gsub("[$,]", "", flow_s$Mean))
flow_n$Mean <- as.numeric(gsub("[$,]", "", flow_n$Mean))



# Sum flows for Postmiles Abs is the same for different stations.
# I believe that they are the onramp information
flow_s <- flow_s %>%
  group_by(Postmile..Abs.) %>%
  summarise(flow = sum(Mean), .groups = "drop")

flow_n <- flow_n %>%
  group_by(Postmile..Abs.) %>%
  summarise(flow = sum(Mean), .groups = "drop")



# all = False because the ones that are not symmetric don't look like they
# follow the pattern of close exits.
flow <- merge(flow_n, flow_s, by = "Postmile..Abs.", all = F)

# Calculate the average flow at a location
flow$ave_flow <- (flow$flow.x + flow$flow.y) / 2

##### Covert from Postmile Abs to Lat Lon #####
post_to_latlon <- post_to_latlon[!duplicated(post_to_latlon[c("Abs_PM")]), ]

pm <- 10.5
Abs_PM <- array(post_to_latlon$Abs_PM)

min(Abs_PM[Abs_PM > pm])
max(Abs_PM[Abs_PM < pm])

calc_lat_lon <- function(pm, Abs_PM) {
  PM_station_above <- min(Abs_PM[Abs_PM > pm])
  PM_station_below <- max(Abs_PM[Abs_PM < pm])

  above_diff <- PM_station_above - pm
  below_diff <- pm - PM_station_below

  above_weight <- above_diff / (above_diff + below_diff)
  below_weight <- below_diff / (above_diff + below_diff)

  new_longitude <- above_weight * 
    post_to_latlon$Longitude[post_to_latlon$Abs_PM == PM_station_above] +
    below_weight * post_to_latlon$Longitude[post_to_latlon$Abs_PM == PM_station_below]
  new_latitude <- above_weight *
    post_to_latlon$Latitude[post_to_latlon$Abs_PM == PM_station_above] +
    below_weight * post_to_latlon$Latitude[post_to_latlon$Abs_PM == PM_station_below]
  return(c(new_longitude, new_latitude))
}


coords <- apply(flow[c("Postmile..Abs.")], 1, calc_lat_lon, Abs_PM)
data <- as.data.frame(cbind(flow$ave_flow, t(coords)))
colnames(data) <- c("Flow", "Longitude", "Latitude")

min(post_to_latlon$Latitude)
min(data$Latitude)
max(post_to_latlon$Latitude)
max(data$Latitude)

min(post_to_latlon$Longitude)
min(data$Longitude)
max(post_to_latlon$Longitude)
max(data$Longitude)

data <- data %>%
  group_by(round(Longitude,3), round(Latitude,3)) %>%
  summarise(Flow = mean(Flow), .groups = "drop")


flow_data <- data
round(flow_data$Longitude[4]) == round(flow_data$Longitude[5])
# write.csv(data, file = "I_15_Flow_Data_2020.csv")

# save as data as RObject
save(flow_data, file = "data-raw/RObject/flow_data.RData")
