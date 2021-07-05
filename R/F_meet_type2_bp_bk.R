#' Number of Encounters Between Bicycle and Pedestrian Walking in the Opposite Direction in Bicycle-Pedestrian Road
#'
#' Number of encounters between bicycle and pedestrian(walking in opposite direction) in cyclists view.
#'     * It follows <Formula 15-9> in KHCM(2013), p.646.
#' @param Q_ped_op Pedestrian traffic flow rate (person/h) walking in the opposite direction to the bicycle
#' @param U_bike Average bike speed (kph)
#' @param U_ped Average walking speed of pedestrian(kph)
#' @keywords bicycle-pedestrian road number of encounters opposite conflict
#' @export F_meet_type2_bp_bk Number of encounters with pedestrians walking in the opposite direction to the direction of bicycle travel (times/h)
#' @examples
#' F_meet_type2_bp_bk(Q_ped_op = 122, U_bike = 11.93, U_ped = 4.32)
F_meet_type2_bp_bk <- function(Q_ped_op = NULL, U_bike = NULL, U_ped = NULL){
  if (Q_ped_op >= 0){
    if (U_bike > 0 & U_ped > 0){f <- Q_ped_op * (U_bike / U_ped + 1)}
    else {f <- 'Error : [U_bike], [U_ped] must be positive(kph). Please check that.'}
  }
  else {f <- 'Error : [Q_ped_op] must be positive(person/h). Please check that.'}
  f
}
