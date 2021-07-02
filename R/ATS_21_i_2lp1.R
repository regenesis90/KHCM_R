#' Average Travel Speed of One-direction, i Section in 2+1 lane road
#'
#' This function follows <Formula 7-15> in KHCM(2013), p.187.
#' @param ATS_up The speed of the one-way basic section of the i-th two-lane road (upstream part of the 2+1 lane road)
#' @param ATS_dn Traffic speed in one direction downstream of the i-th 2+1 lane road
#' @param fs_pl Travel speed correction factor for the i-th overtaking lane section. See \code{\link{fs_pl_2lp1}}
#' @param L_up Analysis section length (m) upstream of the overtaking lane section among the i-th analysis target section
#' @param L_dn The length of the analysis target section before entering the i-th overtaking lane (m)
#' @param L_pl Section length of the i-th overtaking lane (m)
#' @param f_w_ATS_up Lane width and side clearance width of the downstream section of the i-th overtaking lane. See \code{\link{f_w_ATS_2l}}
#' @param f_w_ATS_pl Lane width and side clearance width of the i-th overtaking lane section. See \code{\link{f_w_ATS_2l}}
#' @param f_w_ATS_dn Lane width and side clearance width of the downstream section of the i-th overtaking lane. See \code{\link{f_w_ATS_2l}}
#' @keywords
#' @export ATS_21_i_2lp1 Average travel speed of a 2+1 lane road
#' @examples
ATS_21_i_2lp1 <- function(ATS_up = NULL, ATS_dn = NULL, fs_pl = NULL, L_up = NULL, L_dn = NULL, L_pl = NULL, f_w_ATS_up = NULL, f_w_ATS_dn = NULL, f_w_ATS_pl = NULL){
  if (length(ATS_up) == length(ATS_dn) & length(ATS_up) == length(ATS_dn) & length(ATS_up) == length(fs_pl) & length(ATS_up) == length(L_up) & length(ATS_up) == length(L_dn) & length(ATS_up) == length(L_pl) & length(ATS_up) == length(f_w_ATS_up) & length(ATS_up) == length(f_w_ATS_dn) & length(ATS_up) == length(f_w_ATS_pl)){
    ats_sum <- 0
    for (i in 1:length(ATS_up)){
      p <- (ATS_up[i] - f_w_ATS_up[i])/(ATS_up[i] - f_w_ATS_pl[i])
      q <- (ATS_up[i] * fs_pl[i] * p - f_w_ATS_pl[i])/(ATS_up[i] * fs_pl[i] * p - f_w_ATS_dn[i])
      ats <- (L_up[i] + L_pl[i] + L_dn[i])/((L_up[i] / ATS_up[i]) + (L_pl[i] / (ATS_up[i] * fs_pl[i] * p)) + (L_dn / (ATS_dn * q)))
      ats_sum <- ats_sum + ats
    }
  }
  else {ats_sum <- 'Error : Length of each arguments must be same with each other. Please check that.'}
  ats_sum
}
