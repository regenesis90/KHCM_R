#' Number of Bicycle Encounters in Bidirectional Bicycle-only Road
#'
#' The number of encounters per unit time by the amount of bicycle traffic
#'     running in the opposite direction to the driving direction on the bidirectional bicycle-only road(type1).
#'     * The number of face-to-face encounters(times/h)
#'     * If bidirectional bicycle traffic flow has the same speed distribution, the number of encounters per unit time is double the bicycle traffic flow rate in the opposite direction.
#'     * It follows <Formula 15-3>, <15-8> in KHCM(2013), p.644, p.646.
#' @param Q_bike_op Bicycle traffic flow rate in opposite direction (vph)
#' @keywords bicycle encounter number conflict bidirectional bicycle-only lane
#' @seealso \code{\link{F_pass_type1_bk}}, \code{\link{F_total_type1_bk}}, \code{\link{LOS_type1_bk}}
#' @export F_meet_type1_bk Number of bicycle encounters in bidirectional bicycle-only road(times/h)
#' @examples
#' F_meet_type1_bk(Q_bike_op = 293)
F_meet_type1_bk <- function(Q_bike_op = NULL){
  if (Q_bike_op >= 0){f <- 2 * Q_bike_op}
  else {f <- 'Error : [Q_bike_op] must be positive(vph). Please check that.'}
  f
}
