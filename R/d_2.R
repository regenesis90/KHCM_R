#' Incremental delay(d_2)
#'
#' It follows <Formula 8-50>, <Formula 12-4> in KHCM(2013)
#' @param c The capacity (vph) of the corresponding vehicle group during the analysis period
#' @param t Analysis period length (hours)
#' @param X The saturation of the lane group
#' @export d_2 Incremental lags showing random arrival and supersaturation effects within the analysis period
#' @examples
d_2 <- function(c = NULL, t = NULL, X = NULL){
  900 * t * ((X - 1) + ((X - 1)**2 + (4 * X / (c * T)))**(1/2))
}
