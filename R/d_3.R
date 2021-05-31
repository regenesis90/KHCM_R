#' Additional delays (delays due to oversaturation queues remaining before the analysis period, d_3)
#'
#' It follows <Formula 8-51>, <Formula 8-52>, <Formula 8-53> in KHCM(2013)
#' @param type Choose one from : \code{1}, \code{2}, \code{3}
#' @param Q_b Initial number of vehicles on standby at the beginning of the analysis period (T) (units)
#' @param c The capacity (vph) of the corresponding vehicle group during the analysis period
#' @param V Arrival traffic volume (vph) for the corresponding lane group during the analysis period
#' @param T Analysis period length (hours)
#' @param X The saturation of the lane group
#' @export d_3
#' @examples
d_3 <- function(type = NULL, Q_b = NULL, c = NULL, V = NULL, T = NULL){
  if (type == 1){d3 <- 1800 * Q_b**2 / (c * T * (c - V))}
  if (type == 2){d3 <- (3600 * Q_b / c) - 1800 * T * (1 - X)}
  if (type == 3){d3 <- 3600 * Q_b / c}
  d3
}
