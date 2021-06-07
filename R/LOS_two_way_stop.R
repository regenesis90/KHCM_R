#' Level of Service(LOS) in two-way stop intersection(LOS_two_way_stop)
#'
#' This function follows <Table 10-2> in KHCM(2013) p.470
#' @param avg_d Average operating delay (sec/veh)
#' @keywords
#' @export LOS_two_way_stop
#' @examples
#' LOS_two_way_stop(avg_d = 33.2)
#' LOS_two_way_stop(18.45)
LOS_two_way_stop <- function(avg_d = NULL){
  if (avg_d > 0 & avg_d <= 10){los <- 'A'}
  if (avg_d > 10 & avg_d <= 15){los <- 'B'}
  if (avg_d > 15 & avg_d <= 25){los <- 'C'}
  if (avg_d > 25 & avg_d <= 35){los <- 'D'}
  if (avg_d > 35 & avg_d <= 50){los <- 'E'}
  if (avg_d > 50){los <- 'F'}
  los
}
