#' Additional Delays on Arterial Road
#'
#' Additional delays on arterial road(sec/veh).
#'     It follows <Formula 12-5> in KHCM(2013), p.538.
#' @param type Arterial road type. Choose one from : \code{'type1'}, \code{'type2'}, \code{'type3'}. See \code{\link{type_artl}}
#' @param Q_b Initial number of vehicles on standby at the beginning of the analysis period(vph)
#' @param c The capacity (vph) of the corresponding vehicle group during the analysis period(vph)
#' @param V Arrival traffic volume (vph) for the corresponding lane group during the analysis period
#' @param t Analysis period length (hours)
#' @param X The saturation of the lane group
#' @seealso \code{\link{type_artl}}
#' @export d_3_artl
#' @examples
#' d_3_artl(type = 'type2', Q_b = 12, c = 800, V = 123, t = 1, X = 100)
d_3_artl <- function(type = NULL, Q_b = NULL, c = NULL, V = NULL, t = NULL, X = NULL){
  if (Q_b > 0 & c > 0 & V > 0 & X > 0){
    if (t > 0){
      if (type == 'type1'){d3 <- 1800 * Q_b**2 / (c * t * (c - V))}
      else if (type == 'type2'){d3 <- (3600 * Q_b / c) - 1800 * t * (1 - X)}
      else if (type == 'type3'){d3 <- 3600 * Q_b / c}
    }
    else {d3 <- 'Error : [t] must be positive(hours). Please check that.'}
  }
  else {d3 <- 'Error : [Q_b], [c], [V], [X] must be positive(vph). Please check that.'}
  d3
}
