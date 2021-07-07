#' Capacity for Arterial Road Planning
#'
#' Capacity for arterial road planning(vph).
#'    Capacity value (vph) that requires a rough value on an arterial road or when planning an arterial road
#' @param N Number of lanes going straight at the intersection.
#' @param g_c_ratio Average green time ratio
#' @keywords capacity arterial road planning
#' @export capa_plan_artl The Capacity of Multi-lane Road(pcph)
#' @examples
#' capa_plan_artl(N = 3, g_c_ratio = 0.2)
capa_plan_artl <- function(N = NULL, g_c_ratio = NULL){
  if (N >= 1){
    if (g_c_ratio > 0 & g_c_ratio < 1){
      capa <- 1800 * N * g_c_ratio
    }
    else {capa <- 'Error : [g_c_ratio] must be > 0 and < 1. Please check that.'}
  }
  else {capa <- 'Error : [N] must be positive integer. Please check that.'}
  capa
}
