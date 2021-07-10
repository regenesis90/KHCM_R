#' Capacity Ratio According to the Demand of Each Flow at Unsignalized Intersection
#'
#' Capacity ratio according to the demand of each flow at unsignalized intersections, moving flow i.
#'     It follows <Formula 10-2> in KHCM(2013), p.468.
#' @param v Traffic volume for flow i(pcph)
#' @param c_p Potential capacity for moving flow i(pcph). See \code{\link{capa_p_x_nsi}}
#' @seealso \code{\link{capa_p_x_nsi}}
#' @keywords unsignalized intersection
#' @export x_nsi
#' @examples
#' x_nsi(v = 1200, c_p = 1800)
x_nsi <- function(v = NULL, c_p = NULL){
  if (v >= 0 & c_p > 0){x <- v/c_p}
  else {x <- 'Error : [v], [c_p] must be positive(pcph). Please check that.'}
  x
}
