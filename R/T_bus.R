#' Central bus lane analysis section travel time (T_bus, seconds)
#'
#' It follows <Formula 12-11> in KHCM(2013) p.552.
#' @param t_p_km_arterial_central_bus_lane see function. Total travel time (sec/km) per km of some sections of exclusive median bus lanes or general lanes
#' @param l Length of analysis section (km)
#' @param d Total access delay (s) at all intersections within the analysis target range as a part of the exclusive median bus lane or general lane
#' @keywords
#' @export T_bus
#' @examples
T_bus <- function(t_p_km_arterial_central_bus_lane = NULL, l = NULL, d = NUL){
  t_p_km_arterial_central_bus_lane * l + d
}
