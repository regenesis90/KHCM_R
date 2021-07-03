#' Number of Overtaking on Bicycle Lanes or Basic Section of the Road Bicycle Path
#'
#' Number of overtaking on bicycle lanes or basic section of the road bicycle path(times/h).
#' It follows <Formula 15-1>, <15-2>, <15-14> in KHCM(2013), p.643, 647.
#'     If the bicycle-only road is one-way, the conflict consists only of overtaking.
#'     It is assumed that the speeds of the bicycles follow a normal distribution, and it is assumed that when the bicycles overtake, they do not affect each other's speeds.
#' @param Q_bike_sm Same direction Bicycle traffic flow rate (vph)
#' @param U_bike Average bike speed (kph)
#' @param sd Standard deviation of bicycle speed
#' @keywords
#' @seealso \code{\link{F_meet_bk}}, \code{\link{F_total_bk}}
#' @export F_pass_bk Overtake count in bicycle lanes or basic section of the road bicycle path(times/h)
#' @examples
F_pass_bk <- function(Q_bike_sm = NULL, U_bike = NULL, sd = NULL){
  2 * Q_bike_sm * sd / (U_bike * (pi)**(1/2))
}
