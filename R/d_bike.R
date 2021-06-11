#' Control Delay of bicycle traffic at signal intersections(sec/veh)
#'
#' It follows <Formula 15-15> in KHCM(2013), p.646
#' @param g Valid green time (sec)
#' @param C Signal period(sec)
#' @param V_bike Bicycle traffic flow rate (vph)
#' @param c_bike Bicycle road capacity (vph)
#' @keywords
#' @export d_bike
#' @examples
d_bike <- function(g = NULL, C = NULL, V_bike = NULL, c_bike = NULL){
  if (V_bike/c_bike >= 1){d <- (0.5 * C * (1 - g/C)**2) / (1 - (g/C * V_bike/c_bike))}
  else{d <- (0.5 * C * (1 - g/C)**2) / (1 - (g/C))}
  d
}
