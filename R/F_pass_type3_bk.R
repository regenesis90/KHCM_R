#' Number of Overtaking on Basic Section of City Road, Bicycle Path
#'
#' Number of overtakes by bicycles moving in the same direction on basic section of city road, bicycle path(type3)
#'     * It means number of conflicts(times/h).
#'     * If the bicycle-only road is one-way, the conflict consists only of overtaking.
#'     * It is assumed that the speeds of the bicycles follow a normal distribution, and it is assumed that when the bicycles overtake, they do not affect each other's speeds.
#'     * It follows <Formula 15-13> in KHCM(2013), p.647.
#' @param Q_bike_sm Same direction Bicycle traffic flow rate(vph)
#' @param U_bike Average bike speed (kph)
#' @param sd Standard deviation of bicycle speed
#' @keywords basic section of city road bicycle path conflict number overtaking
#' @seealso \code{\link{LOS_type3_bk}}
#' @export F_pass_type3_bk Overtake count in bicycle lanes or basic section of the road bicycle path(times/h)
#' @examples
#' F_pass_type3_bk(Q_bike_sm = 382)
#' F_pass_type3_bk(Q_bike_sm = 543, U_bike = 12.1, sd = 3.3)
F_pass_type3_bk <- function(Q_bike_sm = NULL, U_bike = NULL, sd = NULL){
  if (Q_bike_sm >= 0){
    if (is.null(U_bike) == TRUE & is.null(sd) == TRUE){
      f <- 0.25 * Q_bike_sm
    }
    else if (U_bike >= 0 & sd >= 0){
      f <- 2 * Q_bike_sm * sd / (U_bike * (pi)**(1/2))
    }
    else {f <- 'Error : [U_bike], [sd] must be all positive or null. Please check that.'}
  }
  else {f <- 'Error : [Q_bike_sm] must be positive(vph). Please check that.'}
  f
}
