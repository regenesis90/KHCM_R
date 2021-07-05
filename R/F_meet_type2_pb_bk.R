#' Number of Encounters Between Bicycle and Pedestrian Walking in the Opposite Direction on Bicycle-Pedestrian Road in Pedestrian's View
#'
#' Number of encounters between bicycle and pedestrian(walking in opposite direction) on bicycle-pedestrian road(type2) in pedestrians view.
#'     * It follows <Formula 15-10> in KHCM(2013), p.646.
#' @param Q_bike_op Bicycle traffic flow rate (vph) in the opposite direction to the pedestrian
#' @param U_bike Average bike speed (kph)
#' @param U_ped Average walking speed of pedestrian(kph)
#' @keywords bicycle-pedestrian road number of encounters opposite conflict
#' @export F_meet_type2_pb_bk Number of encounters between bicycle and pdestrian walking in the opposite direction on bicycle-pedestrian road in pedestrian's view(times/h)
#' @examples
#' F_meet_type2_pb_bk(Q_bike_op = 419, U_bike = 11.93, U_ped = 4.32)
F_meet_type2_pb_bk <- function(Q_bike_op = NULL, U_bike = NULL, U_ped = NULL){
  if (Q_bike_op >= 0){
    if (U_bike > 0 & U_ped > 0){f <- Q_bike_op * (U_bike / U_ped + 1)}
    else {f <- 'Error : [U_bike], [U_ped] must be positive(kph). Please check that.'}
  }
  else {f <- 'Error : [Q_bike_op] must be positive(vph). Please check that.'}
  f
}
