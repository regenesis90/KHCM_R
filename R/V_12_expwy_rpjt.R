#' Traffic volume to the Second Lane at the Ramp-Expressway Junction of Merging Area, Diverging Area
#'
#' This function calculates traffic volume to the second lane in merging area, diverging area.
#'    It follows <Table 4-4>, <Table 4-5> in KHCM(2013), p.89.
#' @param area Area type. Choose from one : \code{'merging'}, \code{'diverging'}
#' @param N Number of main lane. Choose one from: \code{2}, \code{3}, \code{4}.
#' @param type If N = 3, Choose road type from : \code{'independent'}, \code{'continuous'}
#' @param V_F Mains traffic upstream of junctions and classifications (pcph)
#' @param V_R Traffic volume on the ramp route to be analyzed(pcph).
#' @param V_u Traffic on adjacent upstream ramp(pcph).
#' @param V_d Traffic on adjacent downstream ramp(pcph).
#' @param L_u Distance to adjacent upstream ramp(m)
#' @param L_d Distance to adjacent downstream ramp(m)
#' @param L_A Length of acceleration lane(m).
#' @param free_speed_ramp Free speed of ramp road(kph).
#' @export V_12_expwy_rpjt
#' @examples
#' V_12_expwy_rpjt(area = 'merging', N = 3, type = 'independent', V_F = 938, V_R = 281, V_u = 382, L_u = 183, L_A = 100, free_speed = 48)
#' V_12_expwy_rpjt(area = 'diverging', N = 4, V_F = 1247, V_R = 481, V_d = 301, L_d = 120)
V_12_expwy_rpjt <- function(area = NULL, N = NULL, type = NULL, V_F = NULL, V_R = NULL, V_u = NULL, V_d = NULL, L_u = NULL, L_d = NULL, L_A = NULL, free_speed_ramp = NULL){
  if (area == 'merging'){
    P_FM <- P_FM_expwy_rpjt(N = N, type = type, V_F = V_F, V_R = V_R, V_u = V_u, L_u = L_u, L_A = L_A, free_speed_ramp = free_speed_ramp)
    if (is.numeric(P_FM) == TRUE){v12 <- V_F * P_FM}
    else {v12 <- P_FM}
  }
  else if (area == 'diverging'){
    P_FD <- P_FD_expwy_rpjt(N = N, type = type, V_F = V_F, V_R = V_R, V_d = V_d, L_d = L_d)
    if (is.numeric(P_FD) == TRUE){v12 <- V_R + (V_F - V_R) * P_FD}
    else {v12 <- P_FD}
  }
  else {v12 <- 'Error : [area] must be one of [merging] or [diverging]. Please check that.'}
  v12
}
