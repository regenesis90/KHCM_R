#' Average travel speed (kph) in the analysis section of the arterial road with exclusive median bus lanes(avg_speed_total, kph)
#'
#' It follows <Formula 12-12> in KHCM(2013) p.552. 3600 means Conversion factor for converting speed units of m/sec to km/hour
#' @param L section length(km)
#' @param T_bus See T_bus(). Travel time (seconds) of the analysis section of the exclusive median bus lane
#' @param T_others See T_others(). Travel time (seconds) of the general lane analysis section
#' @param V_bus Peak traffic volume (large/hour) in the analysis section of the exclusive median bus lane
#' @param V_others Peak traffic volume of general lane analysis section (large/hour)
#' @keywords
#' @export avg_speed_total
#' @examples
avg_speed_total <- function(L = NULL, T_bus = NULL, T_others = NULL, V_bus = NULL, V_others = NULL){
  vsum <- 0
  vtsum <- 0
  for (i in 1:length(V_bus)){
    v <- V_bus[i] + V_others[i]
    vt <- V_bus[i] * T_bus[i] + V_others[i] * T_others[i]
    vsum <- vsum + v
    vtsum <- vtsum + vt
  }
  3600 * l * vsum / vtsum
}
