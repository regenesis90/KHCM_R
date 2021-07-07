#' Average Travel Speed in the Total Analysis Section of the Arterial Road with Exclusive Central Bus Lane
#'
#' It follows <Formula 12-12> in KHCM(2013) p.552.
#'     3600 means Conversion factor for converting speed units of m/sec to km/hour
#' @param L section length(km)
#' @param T_bus Travel time (seconds) of the analysis section of the exclusive central bus lane
#' @param T_others Travel time (seconds) of the general lane analysis section
#' @param V_bus Peak traffic volume (veh/hour) in the analysis section of the exclusive central bus lane
#' @param V_others Peak traffic volume of general lane analysis section (veh/hour)
#' @keywords average travel speed ats arterial road exclusive central bus lane
#' @seealso \code{\link{T_trv_artl}}, \code{\link{ATS_seg_bus_artl}}
#' @export ATS_total_artl
#' @examples
ATS_total_artl <- function(L = NULL, T_bus = NULL, T_others = NULL, V_bus = NULL, V_others = NULL){
  if (L > 0){
    if (T_bus > 0 & T_others > 0){
      if (V_bus > 0 & V_others > 0){
        vsum <- 0
        vtsum <- 0
        for (i in 1:length(V_bus)){
          v <- V_bus[i] + V_others[i]
          vt <- V_bus[i] * T_bus[i] + V_others[i] * T_others[i]
          vsum <- vsum + v
          vtsum <- vtsum + vt
        }
        ats <- 3600 * L * vsum / vtsum
      }
      else {ats <- 'Error : [V_bus], [V_others] must be positive(veh/hour). Please check that.'}
    }
    else {ats <- 'Error : [T_bus], [T_others] must be positive(seconds). Please check that.'}
  }
  else {ats <- 'Error : [L] must be positive(km). Please check that.'}
  ats
}
