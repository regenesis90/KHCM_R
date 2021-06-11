#' Number of overtaking on one-way bicycle lanes (times/h)
#'
#' It follows <Formula 15-1>, <15-2>, <15-5> in KHCM(2013), p.643, 645
#'     If the bicycle-only road is one-way, the conflict consists only of overtaking.
#'     It is assumed that the speeds of the bicycles follow a normal distribution, and it is assumed that when the bicycles overtake, they do not affect each other's speeds.
#' @param Q_bike_sm Same direction Bicycle traffic flow rate (vph)
#' @param U_bike Average bike speed (kph)
#' @param sd Standard deviation of bicycle speed
#' @keywords
#' @export F_pass Overtake count (times/h)
#' @examples
F_pass <- function(Q_bike_sm = NULL, U_bike = NULL, sd = NULL){
  2 * Q_bike_sm * sd / (U_bike * (pi)**(1/2))
}
