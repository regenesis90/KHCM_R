#' Straight. Ratio of left turns in left-turning common lane groups(P_LT)
#'
#' This function follows <Formula 8-23>, <Formula 8-25> in KHCM(2013)
#' @param lane_type *Categorical* Choose from one : \code{'public_left_turn_lane'}, \code{'integrated_lane'}
#' @param V_L
#' @param V_R
#' @param V_TH
#' @param V_RF
#' @keywords
#' @export P_LT Straight. Ratio of left turns in left-turning common lane groups
#' @examples
#' P_LT(lane_type = 'public_left_turn_lane', V_L = 242, V_TH = 573, V_RF = 201)
P_LT <- function(lane_type = NULL, V_L = NULL, V_R = NULL, V_TH = NULL, V_RF = NULL){
  if (V_L >= 0 & V_TH >= 0 & V_RF >= 0){
    if (lane_type == 'public_left_turn_lane'){p <- V_L / (V_TH - V_RF + V_L)}
    if (lane_type == 'integrated_lane'){p <- V_L / (V_L + V_TH + V_R)}
    p
  }
}
