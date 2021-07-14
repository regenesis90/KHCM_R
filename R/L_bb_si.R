#' Interference Time Due to Bus Stops at Signalized Intersection
#'
#' Interference time due to bus stops at signal intersections.
#'     Time lost per hour due to bus stop (seconds)
#'     This function follows <Formula 8-8> in KHCM(2013), p.231.
#' @param T_b Saturation head time increment according to one bus stop(sec). See \code{\link{T_b_si}}
#' @param l Distance from stop line to bus stop(m)
#' @param V_b Number of bus stops per hour
#' @keywords
#' @seealso \code{\link{T_b_si}}, \code{\link{L_H_si}}
#' @details
#'     * Disruption of flow due to bus stops at a bus stop is affected by the number of stopped buses, stopping time, boarding and disembarking activities, and the location of the bus stop.
#'     * Bus stop obstruction applies only to the lane where bus stopping activity takes place, and there is no effect if the number of bus stops per hour is less than 10.
#'     * The location of the bus stop refers to the distance (m) from the stop line to the bus stop, and if the distance is more than 75 m, there is no obstruction at the bus stop.
#' @export L_bb_si Time lost per hour due to bus stop (seconds)
#' @examples
#' L_bb_si(T_b = 10.8, l = 30, V_b = 12)
L_bb_si <- function(T_b = NULL, l = NULL, V_b = NULL){
  if (T_b >= 0){
    if (V_b >= 0){
      if (l >= 75){
        l_b <- 0
        lb <- T_b * l_b * V_b}
      else if (l > 0 & l < 75){
        l_b <- (75 - l)/75
        lb <- T_b * l_b * V_b}
      else {lb <- 'Error : [l] must be positive(m). Please check that.'}
    }
    else {lb <- 'Error : [V_b] must be positive. Please check that.'}
  }
  else {lb <- 'Error : [T_b] must be >= 0(sec). Please check that.'}
  lb
}
