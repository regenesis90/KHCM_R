#' Uniform Delay When there is No Initial Waiting Vehicle at Signal Intersection
#'
#' Uniform delay when there is no initial waiting vehicle at the signalized intersection.
#'    It follows <Formula 8-47>, <Formula 8-48>, <Formula 8-49> in KHCM(2013), p.254.
#' @param case Choose one from : \code{'case1'}, \code{'case2'}, \code{'case3'}. See \code{\link{d_3_case_si}}
#' @param Q_b Initial number of waiting vehicles (veh)
#' @param C Signal period(seconds)
#' @param g Effective green time allocated to the relevant lane group (sec)
#' @param X The saturation of the lane group
#' @param R Red signal time (sec)
#' @param y Flow rate(= V/S)
#' @param t Analysis period length (hours)
#' @param S Saturated traffic volume of the lane group (vphg)
#' @seealso \code{\link{d_3_case_si}}
#' @export d_1_si Uniform delay
#' @examples
#' d_1_si(Q_b = 0, C = 180, g = 55, X = 0.7)
#' d_1_si(case = 'case1', Q_b = 222, C = 180, g = 55, R = 120, t = 1.5, S = 800, y = 0.6)
#' d_1_si(case = 'case2', Q_b = 222, R = 120)
d_1_si <- function(case = NULL, Q_b = NULL, C = NULL, g = NULL, X = NULL, R = NULL, y = NULL, t = NULL, S = NULL){
  if (Q_b == 0){
    if (g > 0 & C > 0){
      if (X >= 1){d <- (0.5 * c * (1 - (g/C)**2))/(1 - (g/C))}
      else if (X < 1 & X >= 0){d <- (0.5 * C * (1 - (g/C)**2))/(1 - X * (g/C))}
      else {d <- 'Error : [X] must be positive. Please check that.'}
    }
    else {d <- 'Error : [g], [C] must be positive(sec) and [g] != [C]. Please check that.'}
  }
  else {
    if (case == 'case1'){
      if (C > 0 & R > 0){
        if (t > 0){
          if (y > 0){
            if (Q_b > 0){
              if (S > 0){d <- R**2/(2 * C * (1 - y)) + (Q_b * R / (2 * t * S * (1 - y)))}
              else {d <- 'Error : [S] must be positive(vphg). Please check that.'}
            }
            else {d <- 'Error : [Q_b] must be positive(veh). Please check that.'}
          }
          else {d <- 'Error : [y] must be positive(flow ratio). Please check that.'}
        }
        else {d <- 'Error : [t] must be positive(hours). Please check that.'}
      }
      else {d <- 'Error : [C], [R] must be positive(sec). Please check that.'}
    }
    else if (case == 'case2' | case == 'case3'){
      if (R > 0){d <- R/2}
      else {d <- 'Error : [R] must be positive(sec). Please check that.'}
    }
    else {d <- 'Error : [case] must be one of [case1], [case2], [case3]. Please check that.'}
  }
  d
}
