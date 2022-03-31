get_dist <- function(df) {
  key <- key

  dist <- googleway::google_distance(
    origins = list(c(37.000041, -113.622474)),
    destinations = list(c(
      df$Latitude,
      df$Longitude
    )),
    key = key,
    simplify = T
  )

  dist_value <- dist[["rows"]][["elements"]][[1]][["distance"]][["text"]]

  df <- df %>%
    dplyr::mutate(dist = dist_value)
}
