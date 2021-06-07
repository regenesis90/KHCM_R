#' Capacity in allotted lanes (c_SH, pcph)
#'
#' This function follows <Formula 10-4> in KHCM(2013) p.469
#' @param V_l Traffic volume or flow rate (pcph) distributed to left-turn lanes
#' @param V_t Traffic volume or traffic flow rate (pcph) distributed to straight lanes
#' @param V_r Traffic volume or traffic flow rate (pcph) allocated to the right turn lane
#' @param c_ml Left-turn travel capacity allocated to the lane (pcph)
#' @param c_mt Forwarding capacity distributed to lanes (pcph)
#' @param c_mr Right-turn travel capacity allocated to lanes (pcph)
#' @keywords
#' @export c_SH Capacity in allotted lanes (c_SH, pcph)
#' @examples
c_SH <- function(V_l = NULL, V_t = NULL, V_r = NULL, c_ml = NULL, c_mt = NULL, c_mr = NULL){
  (V_l + V_t + V_r) / ((V_l/c_ml) + (V_t/c_mt) + (V_r/c_mr))
}
