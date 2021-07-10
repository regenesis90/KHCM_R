#' Operational Delay for Each Moving Flow x at Unsignalized Intersections
#'
#' Operational delay for each moving flow x at unsignalized intersections(sec/veh).
#'     This function follows <Formula 10-5> in KHCM(2013) p.470
#' @param v Traffic flow rate for moving flow x (vph).
#' @param c_m Capacity of flow x (vph)
#' @param t t Analysis time(1t = 15 minutes = 0.25h)
#' @keywords operational delay unsignalized intersection
#' @seealso \code{\link{LOS_type2_nsi}}
#' @export d_x_nsi
#' @examples
#' d_x_nsi(v = 1234, c_m = 1800, t = 2)
d_x_nsi <- function(v = NULL, c_m = NULL, t = NULL){
  if (v > 0 & c_m > 0){
    if (t > 0){d <- 3600/c_m + 900 * t * ((v/c_m) - 1 + (((v/c_m) - 1)**2 + (3600/c_m)*(v/c_m)/(450 * t))**(1/2)) + 5}
    else {d <- 'Error : [t] must be positive(15 minutes). Please check that.'}
  }
  else {d <- 'Error : [v], [c_m] must be positive(vph). Please check that.'}
  d
}
