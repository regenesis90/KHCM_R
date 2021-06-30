#' Total Delay Rate for i Sections in One Direction on a 2+1 Lane Road
#'
#' Total delay rate(TDR) for i sections in one direction on a 2+1 lane road(%).
#'     This function follows <Formula 7-13> in KHCM(2013), p.185.
#' @param TDR_up Total delay rate of the basic section of the i-th two-lane road in one direction (upstream part of the 2+1 lane road)See TDR().
#' @param TDR_dn Total delay rate in the downstream part of the i-th 2+1 lane road in one direction. See TDR().
#' @param L_up Analysis section length (m) upstream of the overtaking lane section among the i-th analysis target section
#' @param L_dn The length of the analysis target section before entering the i-th overtaking lane (m)
#' @param L_pl Section length of the i-th overtaking lane (m)
#' @param f_pl Total delay rate correction factor for the i-th overtaking lane section. See \code{\link{f_pl_2lp1}}.
#' @param f_w_D_up Lane width and lateral margin correction factor in the upstream section of the i-th overtaking lane. See \code{\link{f_w_D_2l}}.
#' @param f_w_D_pl Lane width and side clearance width correction factor for the i-th overtaking lane section. See \code{\link{f_w_D_2l}}.
#' @param f_w_D_dn Lane width and side clearance width correction factor in the downstream section of the i-th overtaking lane. See \code{\link{f_w_D_2l}}.
#' @keywords TDR Total Delay Rate 2+1 lane road
#' @export TDR_21_i_2lp1 Total Delay Rate for i Sections in One Direction on a 2+1 Lane Road(TDR_2lane_plus1, %)
#' @examples
TDR_21_i_2lp1 <- function(TDR_up = NULL, TDR_dn = NULL, L_up = NULL, L_dn = NULL, L_pl = NULL, f_pl = NULL, f_w_D_up = NULL, f_w_D_dn = NULL, f_w_D_pl = NULL){
  if (length(TDR_up) == length(TDR_dn) & length(TDR_up) == length(L_up) & length(TDR_up) == length(L_dn) & length(TDR_up) == length(L_pl) & length(TDR_up) == length(f_pl) & length(TDR_up) == length(f_w_D_up) & length(TDR_up) == length(f_w_D_dn) & length(TDR_up) == length(f_w_D_pl)){
    tdr_sum <- 0
    for (i in 1:length(TDR_up)){
      tdr <- ((TDR_up[i] * (L_up[i] + f_pl[i] * ((1 + f_w_D_pl[i]/100)/(1 + f_w_D_up[i]/100)) * L_pl[i])) + (TDR_dn[i] * ((1 + f_w_D_pl[i]/100)/(1 + f_w_D_dn[i]/100)) * L_dn[i]))/(L_up[i] + L_dn[i] + L_pl[i])
      tdr_sum <- tdr + tdr_sum
    }
  }
  else {tdr_sum <- 'Error : Length of each arguments must be same with each other. Please check that.'}
  tdr_sum
}
