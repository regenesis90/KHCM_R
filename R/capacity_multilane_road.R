#' The Capacity of Multi-lane Road(pcph)
#'
#' This function follows <Formula 6-2>
#' @param type *Categorical* Type of traffic flow in multi-lane road. Choose one from : \code{'interrupted_flow'}, \code{'uninterrupted_flow'}
#' @param design_speed *Categorical* If \code{type == 'uninterrupted_flow'}, design_speed must be used.
#' @param N *Numeric* Number of lanes going straight at the intersection.
#' @param S *Numeric* Saturation flow(pcphpl)
#' @param g_c_ratio *Numeric* Average green time ratio
#' @keywords capacity weaving section freeway main line link
#' @export capacity_multilane_road The Capacity of Multi-lane Road(pcph)
#' @examples
#' capacity_multilane_road(type = 'uninterrupted_flow', design_speed = 80)
#' capacity_multilane_road(type = 'interrupted_flow', N = 3, S = 2000, g_c_ratio = 0.2)
capacity_multilane_road <- function(type = NULL, design_speed = NULL, N = NULL, S = NULL, g_c_ratio = NULL){
  if (type == 'uninterrupted_flow'){
    if (design_speed == 100){cap <- 2200}
    if (design_speed == 80){cap <- 2000}
  }
  if (type == 'interrupted_flow'){
    if (N >= 1 & S > 0 & g_c_ratio >=0 & g_c_ratio < 1){cap <- N * S * g_c_ratio}
  }
  cap
}
