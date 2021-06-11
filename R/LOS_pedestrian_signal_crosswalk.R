#' Level of Service(LOS) for Pedestrian in Signal Crosswalk
#'
#' It follows <Table 14-6> in KHCM(2013), p.619
#' @param d Average Pedestrian Delay (sec/person)
#' @export LOS_pedestrian_signal_crosswalk
#' @examples
#' LOS_pedestrian_signal_crosswalk(d = 34.2)
#' LOS_pedestrian_signal_crosswalk(84.56)
LOS_pedestrian_signal_crosswalk <- function(d = NULL){
  if (d >= 0 & d <= 15){los <- 'A'}
  if (d > 15 & d <= 30){los <- 'B'}
  if (d > 30 & d <= 45){los <- 'C'}
  if (d > 45 & d <= 60){los <- 'D'}
  if (d > 60 & d <= 90){los <- 'E'}
  if (d > 90){los <- 'F'}
  los
}
