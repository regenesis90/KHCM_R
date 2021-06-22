#' Influence Ratio of Merging Area in Ramp-Expressway Junction
#'
#' This function calculates influence ratio of merging area in ramp-expressway junction.
#'    It follows <Table 4-4> in KHCM(2013), p.89.
#' @param N Number of main lane. Choose one from: \code{2}, \code{3}, \code{4}.
#' @param type If N = 3, Choose road type from : \code{'independent'}, \code{'continuous'}
#' @param V_F Mains traffic upstream of junctions and classifications (pcph)
#' @param V_R Traffic volume on the ramp route to be analyzed(pcph).
#' @param V_u Traffic on adjacent upstream ramp(pcph).
#' @param L_u Distance to adjacent upstream ramp(m)
#' @param L_A Length of acceleration lane(m).
#' @param free_speed_ramp Free speed of ramp road(kph).
#' @export P_FM_expwy_rpjt
#' @examples
#' P_FM_expwy_rpjt(N = 4, V_F = 1200, V_R = 281, V_u = 382, L_u = 320, L_A = 120, free_speed_ramp = 48)
#' P_FM_expwy_rpjt(N = 3, type = 'independent', V_F = 1821, V_R = 333, V_u = 482, L_u = 240, L_A = 200, free_speed_ramp = 35)
P_FM_expwy_rpjt <- function(N = NULL, type = NULL, V_F = NULL, V_R = NULL, V_u = NULL, L_u = NULL, L_A = NULL, free_speed_ramp = NULL){
  if (V_F >= 0 & V_R >= 0 & V_u >= 0 & L_u >= 0 & L_A >= 0 & free_speed_ramp >= 0){
    if (N == 2){pfm <- 1.00}
    else if (N == 3){
      if (type == 'independent'){pfm <- 0.5127 + 0.000193 * V_R}
      else if (type == 'continuous'){pfm <- 0.635 - 0.000022 * (V_R + V_F) - 0.00504 * (V_u/L_u)}
      else {pfm <- 'Error : [type] must be one of [independent] or [continuous]. Please check that.'}
    }
    else if (N == 4){pfm <- 0.094 - 0.0000203 * V_R + 0.0502 * (L_A/free_speed_ramp)}
    else {pfm <- 'Error : [N] must be one of 2, 3, 4. Please check that.'}
  }
  else {pfm <- 'Error : [V_F], [V_R], [V_u], [L_u], [L_A], [free_speed_ramp] must be positive. Please check that.'}
  pfm
}
