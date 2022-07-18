#' Waypoints on a path in two dimensions
#' 
#' Used as an example in *MOSAIC Calculus* in the "Data-driven functions" chapter. 
#'
#' A data frame with `x` and `y` components
#' @export
Robot_stations <- tibble::tribble(
~ x, ~ y,
496, 1109,
1037, 1162,
1251,1109,
1226,889,
902,564,
1034,432,
1197,648,
1389,459,
1170,245,
521,325,
327,541,
277, 948,
492,731,
710,811,
928,868,
737,1060
) |>
  dplyr::mutate(t=dplyr::row_number(), y=1300-y) |>
  dplyr::select(t, x, y)

save(Robot_stations, file = here::here("data/Robot_stations.rda"))
