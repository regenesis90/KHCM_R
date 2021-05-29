#' Total Delay Rate for i Sections in One Direction on a 2+1 Lane Road(TDR_2lane_plus1, %)
#'
#' This function follows <Formula 7-13>
#' @param n *Numeric* Number of 2+1 lane road section.
#' @param TDR_up_i *Numeric* Total delay rate of the basic section of the i-th two-lane road in one direction (upstream part of the 2+1 lane road)See TDR().
#' @param TDR_dn_i *Numeric* Total delay rate in the downstream part of the i-th 2+1 lane road in one direction. See TDR().
#' @param L_up_i *Numeric* Analysis section length (m) upstream of the overtaking lane section among the i-th analysis target section
#' @param L_dn_i *Numeric* The length of the analysis target section before entering the i-th overtaking lane (m)
#' @param L_pl_i *Numeric* Section length of the i-th overtaking lane (m)
#' @param f_pl_i *Numeric* Total delay rate correction factor for the i-th overtaking lane section. See f_pl().
#' @param f_w_D_up_i *Numeric* Lane width and lateral margin correction factor in the upstream section of the i-th overtaking lane. See f_w_D().
#' @param f_w_D_pl_i *Numeric* Lane width and side clearance width correction factor for the i-th overtaking lane section. See f_w_D().
#' @param f_w_D_dn_i *Numeric* Lane width and side clearance width correction factor in the downstream section of the i-th overtaking lane. See f_w_D().
#' @keywords
#' @export TDR_2lane_plus1 Total Delay Rate for i Sections in One Direction on a 2+1 Lane Road(TDR_2lane_plus1, %)
#' @examples
TDR_2lane_plus1 <- function(TDR_up_i = NULL, TDR_dn_i = NULL, L_up_i = NULL, L_dn_i = NULL, L_pl_i = NULL, f_pl_i = NULL, f_w_D_up_i = NULL, f_w_D_dn_i = NULL, f_w_D_pl_i = NULL){
  tdr_sum <- 0
  for (i in 1:n){
    tdr <- (TDR_up_i * (L_up_i + (f_pl_i * ((1 + f_w_D_pl_i/100)/(1 + f_w_D_up_i/100)) * L_pl_i)) + TDR_dn_i * ((1 + f_w_D_pl_i/100)/(1 + f_w_D_dn_i/100)) * L_dn_i)/(L_up_i[i] + L_pl_i[i] + L_dn_i[i])
    tdr_sum <- tdr_sum + tdr
  }
  tdr_sum
}
