
# load libraries, function and data
library(googleway)
source("R/get_dist.R")
load("data-raw/RObject/flow_data.RData")



#rename data
flow_data_list <- flow_data

#add index column to data frame
flow_data_list$index <- 1:nrow(flow_data_list)


#  group into list by index
flow_data_list <- flow_data_list %>%
  dplyr::group_by(index) %>%
  dplyr::group_split()


# compute distance of flow data
flow_data_dist <- do.call(rbind, lapply(flow_data_list, get_dist))

#save data as RObject
save(flow_data_dist, file= "data-raw/RObject/flow_data_dist.RData")


