#' Control Delay of Bicycle Traffic at Signalized Intersections
#'
#' Control Delay of bicycle traffic at signalized intersections(sec/veh).
#'     It is used to evaluate LOS of bicycle traffic at signalized intersections.
#'     It follows <Formula 15-15>, <Formula 15-28> in KHCM(2013), p.646, p.651.
#' @param g Valid green time (sec)
#' @param C Signal period(sec)
#' @param v_bk Bicycle traffic flow rate (vph)
#' @param capa_bk Bicycle road capacity (vph)
#' @keywords Control Delay Bicycle Road Signalized Intersections
#' @export d_bk
#' @examples
#' d_bk(g = 40, C = 175, v_bk = 100, capa_bk = 122)
d_bk <- function(g = NULL, C = NULL, v_bk = NULL, capa_bk = NULL){
  if (g > 0 & C > 0 & v_bk >= 0 & capa_bk > 0){
    if (v_bk/capa_bk >= 1){d <- (0.5 * C * (1 - g/C)**2) / (1 - (g/C * v_bk/capa_bk))}
    else{d <- (0.5 * C * (1 - g/C)**2) / (1 - (g/C))}
  }
  else {d <- 'Error : [g], [C], [v_bk], [capa_bk] must be positive. Please check that.'}
  d
}
