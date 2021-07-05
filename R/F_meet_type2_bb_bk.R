#' Number of Encounters between Bicycles in Bicycle-Pedestrian Road
#'
#' The number of encounters per unit time(times/h) by the amount of bicycle traffic in bicycle-pedestrian road(type2).
#'     * The number of times a bicycle overtakes a bicycle traveling in the same direction on a bicycle-pedestrian road.
#'     * It follows <Formula 15-8> in KHCM(2013), p.646.
#' @param Q_bike_op Bicycle traffic flow rate in opposite direction(vph).
#' @keywords bicycle encounter number conflict bicycle-pedestrian road
#' @seealso \code{\link{F_meet_type2_pb_bk}}, \code{\link{F_meet_type2_bp_bk}}, \code{\link{F_total_type2_b_bk}}, \code{\link{LOS_type2_bk}}
#' @export F_meet_type2_bb_bk The number of encounters per unit time(times/h) by the amount of bicycle traffic in bicycle-pedestrian road(type2)
#' @examples
#' F_meet_type2_bb_bk(Q_bike_op = 293)
F_meet_type2_bb_bk <- function(Q_bike_op = NULL){
  if (Q_bike_op >= 0){f <- 2 * Q_bike_op}
  else {f <- 'Error : [Q_bike_op] must be positive(vph). Please check that.'}
  f
}
