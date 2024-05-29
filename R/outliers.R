
#' @param x A vector of ids of stations whose neighbours are found
#' @param station_loc Data frame of locations of stations,
#' containing three columns: id, latitude, and longitude
#' @param max_num_nbrs maximum number of neighbours to find
#' @param max_dist maximum distance from the target in km
#' @importFrom geosphere distm distHaversine
nbr_subset <- function(id, station_loc, max_num_nbrs = 7, max_dist = 20) {
  # min_num_nbrs = 4
  id_idx <- match(id, station_loc$id)
  .x <- cbind(station_loc$longitude[id_idx],
              station_loc$latitude[id_idx])
  .y <- select(station_loc, longitude, latitude)
  mat_dist <- distm(.x, .y, distHaversine)
  colnames(mat_dist) <- station_loc$id
  rownames(mat_dist) <- id

  df_nbr <- as_tibble(mat_dist, rownames = "target") %>%
    pivot_longer(-target, names_to = "neighbour", values_to = "dist") %>%
    group_by(target) %>%
    filter(dist < max_dist * 1e3, dist>0) %>%
    slice_min(dist, n = max_num_nbrs) %>%
    ungroup()

  df_nbr
}
