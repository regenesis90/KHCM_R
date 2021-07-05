#' Total Number of Conflicts for Pedestrians in Bicycle-Pedestrian Road
#'
#' Total number of conflicts on the bike-pedestrian road(type2) in pedestrians view.
#'     It follows <Formula 15-11>, <Formula 15-25> in KHCM(2013), p.646, p.651.
#' @param Q_bike_sm Same direction Bicycle traffic flow rate (vph)
#' @param Q_bike_op Bicycle traffic flow rate in opposite direction (vph)
#' @param U_bike Average bike speed (kph)
#' @param U_ped Average walking speed (kph)
#' @keywords bicycle-pedestrian road total number of conflicts pedestrian
#' @seealso \code{\link{F_pass_type2_pb_bk}}, \code{\link{F_meet_type2_pb_bk}}
#' @export F_total_type2_p_bk Total number of conflicts on the bike-pedestrian road(type2) in pedestrians view(times/h)
#' @examples
#' F_total_type2_p_bk(Q_bike_sm = 239, Q_bike_op = 382)
#' F_total_type2_p_bk(Q_bike_sm = 239, Q_bike_op = 382, U_bike = 12.33, U_ped = 5.03)
F_total_type2_p_bk <- function(Q_bike_sm = NULL, Q_bike_op = NULL, U_bike = NULL, U_ped = NULL){
  if (Q_bike_sm >= 0 & Q_bike_op >= 0){
    if (is.null(U_bike) == TRUE & is.null(U_ped) == TRUE){
      f <- (Q_bike_sm + Q_bike_op) * 2/3
    }
    else {
      if (U_bike > 0 & U_ped > 0){
        F_pass_pb <- F_pass_type2_pb_bk(Q_bike_sm = Q_bike_sm, U_bike = U_bike, U_ped = U_ped)
        F_meet_pb <- F_meet_type2_pb_bk(Q_bike_op = Q_bike_op, U_bike = U_bike, U_ped = U_ped)
        f <- F_pass_pb + 0.5 * (F_meet_pb)
      }
      else {f <- 'Error : [U_bike], [U_ped] must be positive. Please check that.'}
    }
  }
  else {f <- 'Error : [Q_bike_sm], [Q_bike_op] must be positive(vph). Please check that.'}
  f
}
