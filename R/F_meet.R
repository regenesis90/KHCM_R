#' Number of bicycles in the opposite direction (times/h)
#'
#' It follows <Formula 15-3>, <15-8> in KHCM(2013), p.644, p.646
#'     The number of face-to-face encounters is proportional to the amount of bicycle traffic running in the opposite direction to the driving direction.
#'     If the two-way bicycle traffic flow has the same speed distribution, the number of encounters per unit time is double the bicycle traffic flow rate in the opposite direction.
#' @param Q_bike_op Bicycle traffic flow rate in opposite direction (vph)
#' @keywords
#' @export F_meet Number of bicycles in the opposite direction (times/h)
#' @examples
F_meet <- function(Q_bike_op = NULL){
  2 * Q_bike_op
}
