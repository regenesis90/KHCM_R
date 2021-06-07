#' Roundabout ramp capacity(c_roundabout, pcph)
#'
#' It follows <Formula 11-2> in KHCM(2013) p.493
#' @param V_EBL East-turn left-entry traffic (vph)
#' @param V_EBT Traffic volume going straight in the east direction (vph)
#' @param V_SBL South-facing left turn traffic volume (vph)
#' @param V_U_NB Sum of U-turn traffic on each approach except for northbound U-turn traffic (vph)
#' @export V_c_NB Conflicting traffic volume (vph) of northbound traffic flow
#' @examples
V_c_NB <- function(V_EBL = NULL, V_EBT = NULL, V_SBL = NULL, V_U_NB = NULL){
  V_EBL + V_EBT + V_SBL + V_U_NB
}
