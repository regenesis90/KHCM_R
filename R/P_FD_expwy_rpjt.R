#' Influence Ratio of diverging Area in Ramp-Expressway Junction
#'
#' This function calculates influence ratio of diverging area in ramp-expressway junction.
#'    It follows <Table 4-5> in KHCM(2013), p.89.
#' @param N Number of main lane. Choose one from: \code{2}, \code{3}, \code{4}.
#' @param type If N = 3, Choose road type from : \code{'independent'}, \code{'continuous'}
#' @param V_F Mains traffic upstream of junctions and classifications (pcph)
#' @param V_R Traffic volume on the ramp route to be analyzed(pcph).
#' @param V_d Traffic on adjacent downstream ramp(pcph).
#' @param L_d Distance to adjacent downstream ramp(m)
#' @export P_FD_expwy_rpjt
#' @examples
#' P_FD_expwy_rpjt(N = 4, V_F = 2184, V_R = 382, V_d = 593, L_d = 288)
#' P_FD_expwy_rpjt(N = 3, type = 'continuous', V_F = 1893, V_R = 942, V_d = 392, L_d = 392)
P_FD_expwy_rpjt <- function(N = NULL, type = NULL, V_F = NULL, V_R = NULL, V_d = NULL, L_d = NULL){
  if (V_F >= 0 & V_R >= 0 & V_d >= 0 & L_d >= 0){
    if (N == 2){pfd <- 1.00}
    else if (N == 3){
      if (type == 'independent'){pfd <- 0.609 - 0.0000004 * V_F - 0.00015 * V_R}
      else if (type == 'continuous'){pfd <- 0.7960 - 0.0000758 * V_F + 0.0259 * (V_d/L_d)}
      else {pfd <- 'Error : [type] must be one of [independent] or [continuous]. Please check that.'}
    }
    else if (N == 4){pfd <- 0.453}
    else {pfd <- 'Error : [N] must be one of 2, 3, 4. Please check that.'}
  }
  else {pfd <- 'Error : [V_F], [V_R], [V_d], [L_d] must be positive. Please check that.'}
  pfd
}
