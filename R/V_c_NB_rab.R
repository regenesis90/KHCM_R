#' Conflict Traffic Volume in Roundabout
#'
#' Conflict traffic volume(vph)
#'     according to the traffic volume of the north approach approach at the roundabout.
#'     If there is a U-turn vehicle on each approach,
#'     the U-turn traffic volume excluding the northbound traffic flow is included
#'     in the conflicting traffic volume of the northbound traffic flow.
#'     In addition, the conflicting traffic volume can be directly investigated and reflected in the conflicting area.
#'     It follows <Formula 11-2>, <Formula 11-7 in KHCM(2013) p.493, 501.
#' @param V_EBL East-turn left-entry traffic (vph)
#' @param V_EBT Traffic volume going straight in the east direction (vph)
#' @param V_SBL South-facing left turn traffic volume (vph)
#' @param V_U_NB Sum of U-turn traffic on each approach except for northbound U-turn traffic (vph)
#' @export V_c_NB_rab Conflicting traffic volume (vph) of northbound traffic flow
#' @examples
#' V_c_NB_rab(V_EBL = 100, V_EBT = 121, V_SBL = 32, V_U_NB = 94)
V_c_NB_rab <- function(V_EBL = NULL, V_EBT = NULL, V_SBL = NULL, V_U_NB = NULL){
  if (V_EBL >= 0 & V_EBT >= 0 & V_SBL >= 0 & V_U_NB >= 0){res <- V_EBL + V_EBT + V_SBL + V_U_NB}
  else {res <- 'Error : [V_EBL], [V_EBT], [V_SBL], [V_U_NB] must be >= 0(vph). Please check that.'}
  res
}
