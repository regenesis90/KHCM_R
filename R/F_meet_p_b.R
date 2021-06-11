#' Number of encounters with bicycles moving in the opposite direction to the direction seen from the pedestrian's point of view (times/h)
#'
#' It follows <Formula 15-10> in KHCM(2013), p.646
#' @param Q_bike_op Bicycle traffic flow rate in opposite direction (vph)
#' @param U_bike Average bike speed (kph)
#' @param U_ped Average walking speed (kph)
#' @keywords
#' @export F_meet_p_b Number of encounters with pedestrians running in the opposite direction to the direction of bicycle travel (times/h)
#' @examples
F_meet_p_b <- function(Q_bike_op = NULL, U_bike = NULL, U_ped = NULL){
  Q_bike_op * (U_ped / U_bike + 1)
}
