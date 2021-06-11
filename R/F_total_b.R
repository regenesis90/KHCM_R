#' Total number of collisions of bicycles on two-way bicycle lanes (times/h)
#'
#' It follows <Formula 15-11> in KHCM(2013), p.646
#' @param Q_bike_sm Same direction Bicycle traffic flow rate (vph)
#' @param Q_bike_op Bicycle traffic flow rate in opposite direction (vph)
#' @param Q_ped_sm Same direction pedestrian traffic flow rate (vph)
#' @param Q_ped_op Pedestrian traffic flow rate (person/h) walking in the opposite direction to the bicycle
#' @param U_bike Average bike speed (kph)
#' @param U_ped Average walking speed (kph)
#' @param sd Standard deviation of bicycle speed
#' @keywords
#' @export F_total_b
#' @examples
F_total_b <- function(Q_bike_sm = NULL, Q_bike_op = NULL, Q_ped_sm = NULL, Q_ped_op = NULL, U_bike = NULL, U_ped = NULL, sd = NULL){
  ((2 * Q_bike_sm * sd)/(U_bike * (pi)**(1/2))) + Q_ped_sm * (U_bike/U_ped - 1) + 0.5 * ((2 * Q_bike_op) + Q_ped_op * (1 + U_bike/U_ped))
}
