#' Incremental Lag Indicative of Random Arrivals and Supersaturation on Arterial Road
#'
#' Incremental delay showing random arrivals and oversaturation on arterial roads
#'     It follows <Formula 12-4> in KHCM(2013), p.538.
#' @param c The capacity (vph) of the corresponding vehicle group during the analysis period
#' @param t Analysis period length (hours)
#' @param X The saturation of the lane group
#' @export d_2_artl Incremental lags showing random arrival and supersaturation effects within the analysis period
#' @examples
#' d_2_artl(c = 120, t = 0.1, X = 12)
d_2_artl <- function(c = NULL, t = NULL, X = NULL){
  if (c > 0){
    if (t > 0){
      if (X > 0){d2 <- 900 * t * ((X - 1) + ((X - 1)**2 + (4 * X / (c * t)))**(1/2))}
      else {d2 <- 'Error : [X] must be positive. Please check that.'}
    }
    else {d2 <- 'Error : [t] must be positive(hours). Please check that.'}
  }
  else {d2 <- 'Error : [c] must be positive(vph). Please check that.'}
  d2
}
