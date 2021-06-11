#' Total number of collisions of pedestrians on two-way bicycle lanes (times/h)
#'
#' It follows <Formula 15-12> in KHCM(2013), p.646
#' @param Q_bike_sm Same direction Bicycle traffic flow rate (vph)
#' @param Q_bike_op Bicycle traffic flow rate in opposite direction (vph)
#' @param U_bike Average bike speed (kph)
#' @param U_ped Average walking speed (kph)
#' @keywords
#' @export F_total_p
#' @examples
F_total_p <- function(Q_bike_sm = NULL, Q_bike_op = NULL, U_bike = NULL, U_ped = NULL){
  Q_bike_sm * (1 - (U_ped / U_bike)) + 0.5 * (Q_bike_op * (1 + U_ped / U_bike))
}
