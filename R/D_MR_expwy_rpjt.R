#' Average Density of the Merging Area of Influence
#'
#' This function calculates average density of the merging area in ramp-expressway junction(pcpkmpl)
#'    It follows the formula in KHCM(2013), p.90.
#' @param N Number of main lane. Choose one from: \code{2}, \code{3}, \code{4}.
#' @param type If N = 3, Choose road type from : \code{'independent'}, \code{'continuous'}
#' @param V_F Mains traffic upstream of junctions and classifications (pcph)
#' @param V_R Traffic volume on the ramp route to be analyzed(pcph).
#' @param V_u Traffic on adjacent upstream ramp(pcph).
#' @param L_u Distance to adjacent upstream ramp(m)
#' @param L_A Length of acceleration lane(m).
#' @param free_speed_ramp Free speed of ramp road(kph).
#' @export D_MR_expwy_rpjt
#' @examples
#' D_MR_expwy_rpjt(N = 2, V_F = 1932, V_R = 291, V_u = 284, L_u = 135, L_A = 100, free_speed_ramp = 52)
D_MR_expwy_rpjt <- function(N = NULL, type = NULL, V_F = NULL, V_R = NULL, V_u = NULL, L_u = NULL, L_A = NULL, free_speed_ramp = NULL){
  V_12 <- V_12_expwy_rpjt(area = 'merging', N = N, type = type, V_F = V_F, V_R = V_R, V_u = V_u, V_d = V_d, L_u = L_u, L_d = L_d, L_A = L_A, free_speed_ramp = free_speed_ramp)
  if (is.numeric(V_12) == TRUE){D_MR <- 0.2048 + 0.003185 * V_R + 0.005989 * V_12 - 0.00101 * L_A}
  else {D_MR <- V_12}
  D_MR
}
