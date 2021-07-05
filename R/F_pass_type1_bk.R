#' Number of Overtaking on Bicycle-only Road
#'
#' Number of overtakes by bicycles moving in the same direction on bicycle-only road(type1).
#'     * It means number of conflicts(times/h).
#'     * If the bicycle-only road is one-way, the conflict consists only of overtaking.
#'     * It is assumed that the speeds of the bicycles follow a normal distribution, and it is assumed that when the bicycles overtake, they do not affect each other's speeds.
#'     * It follows <Formula 15-1>, <15-2> in KHCM(2013), p.643.
#' @param road Road direction. Choose one from :\code{'one-way'}, \code{'bidirectional'}
#' @param Q_bike_sm Same direction Bicycle traffic flow rate(vph)
#' @param U_bike Average bike speed (kph)
#' @param sd Standard deviation of bicycle speed
#' @param p If \code{road == 'bidirectional'}, \code{p} means proportion of bicycle traffic in the direction of travel in total (bidirectional) bicycle traffic
#' @keywords bicycle-only road conflict number overtaking
#' @seealso \code{\link{F_meet_type1_bk}}, \code{\link{F_total_type1_bk}}, \code{\link{LOS_type1_bk}}
#' @export F_pass_type1_bk Overtake count in bicycle lanes or basic section of the road bicycle path(times/h)
#' @examples
#' F_pass_type1_bk(road = 'one-way', Q_bike_sm = 382)
#' F_pass_type1_bk(road = 'one-way', Q_bike_sm = 543, U_bike = 12.1, sd = 3.3)
#' F_pass_type1_bk(road = 'bidirectional', Q_bike_sm = 555, p = 0.43)
#' F_pass_type1_bk(road = 'bidirectional', Q_bike_sm = 194, U_bike = 9.1, sd = 5.32)
F_pass_type1_bk <- function(road = NULL, Q_bike_sm = NULL, U_bike = NULL, sd = NULL, p = NULL){
  if (Q_bike_sm >= 0){
    if (road == 'one-way'){
      if (is.null(U_bike) == TRUE & is.null(sd) == TRUE){
        f <- 0.25 * Q_bike_sm
      }
      else if (U_bike >= 0 & sd >= 0){
        f <- 2 * Q_bike_sm * sd / (U_bike * (pi)**(1/2))
      }
      else {f <- 'Error : [U_bike], [sd] must be all positive or null. Please check that.'}
    }
    else if (road == 'bidirectional'){
      if (is.null(U_bike) == TRUE & is.null(sd) == TRUE){
        if (p >= 0 & p <= 1){f <- Q_bike_sm * (1 - 0.75 * p)}
        else {f <- 'Error : [p] must be >= 0 and <= 1. Please check that.'}
      }
      else if (U_bike >= 0 & sd >= 0){
        f <- 2 * Q_bike_sm * sd / (U_bike * (pi)**(1/2))
      }
      else {f <- 'Error : [U_bike], [sd] must be all positive or null. Please check that.'}
    }
    else {f <- 'Error : [road] must be one of [one-way] or [bidirectional]. Please check that.'}
  }
  else {f <- 'Error : [Q_bike_sm] must be positive(vph). Please check that.'}
  f
}
