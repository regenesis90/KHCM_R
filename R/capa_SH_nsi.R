#' Capacity Considering Lane Distribution at Two-way Stop Unsignalized Intersections
#'
#' Capacity (pcph) considering lane distribution at two-way stop intersections(type2) among unsignalized intersection.
#'     This function follows <Formula 10-4> in KHCM(2013) p.469.
#' @param v_l Traffic volume or flow rate (pcph) distributed to left-turn lanes
#' @param v_t Traffic volume or traffic flow rate (pcph) distributed to straight lanes
#' @param v_r Traffic volume or traffic flow rate (pcph) allocated to the right turn lane
#' @param c_ml Left-turn travel capacity allocated to the lane (pcph).
#' @param c_mt Forwarding capacity distributed to lanes (pcph).
#' @param c_mr Right-turn travel capacity allocated to lanes (pcph).
#' @keywords capacity considering lane distribution two-way stop unsignalized non-signalized intersections
#' @details
#'      * In the bidirectional stop intersection analysis, it is assumed that each sub-road flow has an independent lane,
#'        but in reality, this is often not the case.
#'      * There are many cases where two or three types of moving flow are used by distributing the lanes in one lane.
#'        Therefore, in order to correct this situation, the capacity considering the lane distribution is used.
#' @export capa_SH_nsi
#' @examples
capa_SH_nsi <- function(v_l = NULL, v_t = NULL, v_r = NULL, c_ml = NULL, c_mt = NULL, c_mr = NULL){
  if (v_l > 0 & v_t > 0 & v_r > 0){
    if (c_ml > 0 & c_mt > 0 & c_mr > 0){
      ca <- (V_l + V_t + V_r) / ((V_l/c_ml) + (V_t/c_mt) + (V_r/c_mr))
    }
    else {ca <- 'Error : [c_ml], [c_mt], [c_mr] must be positive(pcph). Please check that.'}
  }
  else {ca <- 'Error : [v_l], [v_t], [v_r] must be positive(pcph). Please check that.'}
  ca
}
