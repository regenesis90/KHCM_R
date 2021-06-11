#' Number of encounters with pedestrians running in the opposite direction to the direction of bicycle travel (times/h)
#'
#' It follows <Formula 15-9> in KHCM(2013), p.646
#' @param Q_ped_op Pedestrian traffic flow rate (person/h) walking in the opposite direction to the bicycle
#' @param U_bike Average bike speed (kph)
#' @param U_ped Average walking speed (kph)
#' @keywords
#' @export F_meet_b_p Number of encounters with pedestrians running in the opposite direction to the direction of bicycle travel (times/h)
#' @examples
F_meet_b_p <- function(Q_ped_op = NULL, U_bike = NULL, U_ped = NULL){
  Q_ped_op * (U_bike / U_ped + 1)
}
