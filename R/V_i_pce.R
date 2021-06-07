#' i Car traffic per hour on the ramp in roundabout(V_i_pce, pcph)
#'
#' It follows <Formula 11-4> in KHCM(2013) p.500
#' @param V_p_i i ramp traffic volume (vph)
#' @param lane Choose one from L \code{1}, \code{2}
#' @param hv_ratio Heavy Vehicle Ratio(%)
#' @param P_T Heavy Vehicle Mixing Ratio
#' @export V_i_pce
#' @examples
V_i_pce <- function(lane = NULL, hv_ratio = NULL, P_T = NULL){
  fhv <- f_hv_roundabout(lane = lane, hv_ratio = hv_ratio, P_T = P_T)
  V_p_i / fhv
}
