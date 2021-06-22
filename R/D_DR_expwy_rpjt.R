#' Average Density of the Diverging Area of Influence
#'
#' This function calculates average density of the diverging area in ramp-expressway junction(pcpkmpl)
#'    It follows the formula in KHCM(2013), p.90.
#' @param N Number of main lane. Choose one from: \code{2}, \code{3}, \code{4}.
#' @param type If N = 3, Choose road type from : \code{'independent'}, \code{'continuous'}
#' @param V_F Mains traffic upstream of junctions and classifications (pcph)
#' @param V_R Traffic volume on the ramp route to be analyzed(pcph).
#' @param V_d Traffic on adjacent downstream ramp(pcph).
#' @param L_d Distance to adjacent downstream ramp(m).
#' @param L_D Length of deceleration lane(m).
#' @export D_DR_expwy_rpjt
#' @examples
#' D_DR_expwy_rpjt(N = 3, type = 'continuous', V_F = 1932, V_R = 291, V_d = 291, L_d = 120, L_D = 100)
D_DR_expwy_rpjt <- function(N = NULL, type = NULL, V_F = NULL, V_R = NULL, V_d = NULL, L_d = NULL, L_D = NULL){
  V_12 <- V_12_expwy_rpjt(area = 'diverging', N = N, type = type, V_F = V_F, V_R = V_R, V_u = V_u, V_d = V_d, L_u = L_u, L_d = L_d, L_A = L_A, free_speed_ramp = free_speed_ramp)
  if (is.numeric(V_12) == TRUE){D_DR <- 0.5108 + 0.00589 * V_12 - 0.0043 * L_D}
  else {D_DR <- V_12}
  D_DR
}
