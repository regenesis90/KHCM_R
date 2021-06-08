#' General lane analysis section travel time (T_bus, seconds)
#'
#' It follows <Formula 12-11> in KHCM(2013) p.552.
#' @param t_p_km_arterial see function. Total travel time (sec/km) per km of some sections of general lanes
#' @param l Length of analysis section (km)
#' @param d Total access delay (s) at all intersections within the analysis target range as a part of the general lane
#' @keywords
#' @export T_other
#' @examples
T_other <- function(t_p_km_arterial = NULL, l = NULL, d = NUL){
  t_p_km_arterial * l + d
}
