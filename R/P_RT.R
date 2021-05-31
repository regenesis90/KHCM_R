#' Straight. Ratio of left turns in right-turning common lane groups(P_RT)
#'
#' This function follows <Formula 8-24>, <Formula 8-25> in KHCM(2013)
#' @param lane_type *Categorical* Choose from one : \code{'public_left_turn_lane'}, \code{'integrated_lane'}
#' @param V_R
#' @param V_TH
#' @param V_LF
#' @param V_L
#' @keywords
#' @export P_RT Straight. Ratio of left turns in right-turning common lane groups
#' @examples
#' P_RT('public_left_turn_lane', 472, 382, 532, 222)
P_RT <- function(lane_type = NULL, V_L = NULL, V_R = NULL, V_TH = NULL, V_LF = NULL){
  if (V_R >= 0 & V_TH >= 0 & V_LF >= 0){
    if (lane_type == 'public_left_turn_lane'){p <- V_R / (V_TH - V_LF + V_R)}
    if (lane_type == 'integrated_lane'){p <- V_R / (V_L + V_TH + V_R)}
    p
  }
}
