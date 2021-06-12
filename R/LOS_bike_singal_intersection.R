#' Service level of cyclists at signal intersections
#'
#' It follows <Table 15-6> in KHCM(2013), p.655
#' @param d Control delay(seconds/veh). See d_bike()
#' @keywords
#' @export LOS_bike_signal_intersection
#' @examples
#' LOS_bike_signal_intersection(32.73)
LOS_bike_signal_intersection <- function(d = NULL){
  if (d >= 0 & d < 8){los <- 'A'}
  if (d >= 8 & d < 12){los <- 'B'}
  if (d >= 12 & d < 25){los <- 'C'}
  if (d >= 25 & d < 40){los <- 'D'}
  if (d >= 40 & d < 55){los <- 'E'}
  if (d >= 55){los <- 'F'}
  los
}
