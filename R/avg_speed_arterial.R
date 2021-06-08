#' Average travel speed on arterial roads(avg_speed_arterial, kph)
#'
#' It follows <Formula 12-1> in KHCM(2013) p.535. 3600 means Conversion factor for converting speed units of m/sec to km/hour
#' @param L section length(km)
#' @param t_p_km_arterial Total traveling time per km of all or part of the arterial road (sec/km). See t_p_km_arterial()
#' @param total_d Total approach delay (seconds) at intersections within the scope of analysis for all or part of the arterial road
#' @keywords
#' @export avg_speed_arterial
#' @examples
avg_speed_arterial <- function(L = NULL, t_p_km_arterial = NULL, total_d = NULL){
  3600 * L / (t_p_km_arterial * L + total_d)
}
