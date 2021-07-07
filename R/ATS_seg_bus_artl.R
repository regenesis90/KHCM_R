#' Average Travel Speed for Each Analysis Section of Arterial Roads with Exclusive Central Bus Lanes
#'
#' Average travel speed (kph) for each analysis section of arterial roads with exclusive central bus lanes.
#'     It follows <Formula 12-10> in KHCM(2013) p.551.
#'     3600 means Conversion factor for converting speed units of m/sec to km/hour
#' @param L Section length(km)
#' @param T_bus Travel time (seconds) of the analysis section of the exclusive central bus lane
#' @param T_others Travel time (seconds) of the general lane analysis section
#' @param V_bus Peak hour traffic volume (veh/hour) in the analysis section of the exclusive central bus lane
#' @param V_others Peak hour traffic volume of general lane analysis section (veh/hour)
#' @keywords Average travel speed ATS exclusive central bus lane
#' @seealso \code{\link{T_trv_artl}}
#' @export ATS_seg_bus_artl
#' @examples
#' ATS_seg_bus_artl(L = 1.2, T_bus = 30, T_others = 102, V_bus = 100, V_others = 1000)
ATS_seg_bus_artl <- function(L = NULL, T_bus = NULL, T_others = NULL, V_bus = NULL, V_others = NULL){
  if (L > 0){
    if (T_bus > 0 & T_others > 0){
      if (V_bus > 0 & V_others > 0){
        ats <- 3600 * (V_bus + V_others) * L / (V_bus * T_bus + V_others * T_others)}
      else {ats <- 'Error : [V_bus], [V_others] must be positive(seconds). Please check that.'}
    }
    else {ats <- 'Error : [T_bus], [T_others] must be positive(seconds). Please check that.'}
  }
  else {ats <- 'Error : [L] must be positive(km). Please check that.'}
  ats
}
