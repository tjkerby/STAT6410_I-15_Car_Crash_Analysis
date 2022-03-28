get_dist <- function(df) {
  key <- "api_key"

  dist <- googleway::google_distance(
    origins = list(c(37.05840, -113.5845)),
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
