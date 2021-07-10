#' Resistance Coefficient According to Disturbance Flow at Unsignalized Intersection
#'
#' Resistance coefficient according to disturbance flow at unsignalized intersection.
#'     It follows <Formula 10-3>, <Figure 10-9> in KHCM(2013), p.469.
#' @param x Capacity ratio according to the demand of each flow at unsignalized intersections, moving flow i. See \code{\link{x_nsi}}
#' @keywords resistance coefficient disturbance flow unsignalized intersection
#' @seealso \code{\link{x_nsi}}
#' @export p_nsi
#' @examples
#' p_nsi(x = 0.44)
p_nsi <- function(x = NULL){
  if (x >= 0){p <- -0.04 * x**2 - 0.64 * x + 1}
  else {p <- 'Error : [x] must be positive. Please check that.'}
  p
}
