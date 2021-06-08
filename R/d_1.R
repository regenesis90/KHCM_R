#' Uniform delay(d_1)
#'
#' It follows <Formula 8-47>, <Formula 8-48>, <Formula 8-49>, <Formula 12-3> in KHCM(2013)
#' @param type *Categorical* Choose one from : \code{1}, \code{2}, \code{3}
#' @param Q_b Initial number of waiting vehicles (units)
#' @param C Signal period(seconds)
#' @param g Effective green time allocated to the relevant lane group (seconds)
#' @param X The saturation of the lane group
#' @param R Red signal time (sec)
#' @param y Flow rate(= V/S)
#' @param T Analysis period length (hours)
#' @param S Saturated traffic volume of the lane group (vphg)
#' @export d_1 Uniform delay
#' @examples
d_1 <- function(type = NULL, Q_b = NULL, C = NULL, g = NULL, X = NULL, R = NULL, y = NULL, T = NULL, S = NULL){
  if (Q_b == 0){
    if (X >= 1){d1 <- (0.5 * C * (1 - g/C)**2)/(1 - X * g/C)}
    if (X < 1){d1 <- (0.5 * C * (1 - g/C)**2)/(1 - g/C)}
  }
  if (type == 1){d1 <- (R**2/(2 * C * (1 - y))) + (Q_b * R / (2 * T * S * (1 - y)))}
  if (type == 2 | type == 3){d1 <- R / 2}
  d1
}
