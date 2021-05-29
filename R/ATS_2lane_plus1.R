#' Equation for calculating the Average travel speed of a 2+1 lane road(ATS_2lane_plus1, kph)
#'
#' This function follows <Formula 7-15>
#' @param n *Numeric* Number of 2+1 lane road section.
#' @param ATS_up_i *Numeric* =TDR_d. The speed of the one-way basic section of the i-th two-lane road (upstream part of the 2+1 lane road)
#' @param ATS_dn_i *Numeric* Traffic speed in one direction downstream of the i-th 2+1 lane road
#' @param fs_pl_i *Numeric* Traffic speed correction factor for the i-th overtaking lane section (<Table 7-15>)
#' @param L_up_i *Numeric* Analysis section length (m) upstream of the overtaking lane section among the i-th analysis target section
#' @param L_dn_i *Numeric* The length of the analysis target section before entering the i-th overtaking lane (m)
#' @param L_pl_i *Numeric* Section length of the i-th overtaking lane (m)
#' @param f_w_ATS_up_i *Numeric*
#' @param f_w_ATS_pl_i *Numeric* Lane width and side clearance width of the i-th overtaking lane section
#' @param f_w_ATS_dn_i *Numeric* Lane width and side clearance width of the downstream section of the i-th overtaking lane
#' @keywords
#' @export ATS_2lane_plus1 Average travel speed of a 2+1 lane road
#' @examples
ATS_2lane_plus1 <- function(n = NULL, ATS_up_i = NULL, ATS_dn_i = NULL, fs_pl_i = NULL, L_up_i = NULL, L_dn_i = NULL, L_pl_i = NULL, f_w_ATS_up_i = NULL, f_w_ATS_dn_i = NULL, f_w_ATS_pl_i = NULL){
  ats_sum <- 0
  for (i in 1:n){
    p <- (ATS_up_i - f_w_ATS_up_i)/(ATS_up_i - f_w_ATS_pl_i)
    q <- (ATS_up_i * fs_pl_i * p - f_w_ATS_pl_i)/(ATS_up_i * fs_pl_i * P - f_w_ATS_dn_i)
    ats <- (L_up_i + L_pl_i + L_dn_i)/((L_up_i / ATS_up_i) + (L_pl_i / (ATS_up_i * fs_pl_i * p)) + (L_dn_i / (ATS_dn_i * q)))
    ats_sum <- ats_sum + ats
  }
  ats_sum
}
