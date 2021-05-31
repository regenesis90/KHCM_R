#' Saturated traffic flow rate correction formula and coefficients for road occupancy construction sections in the upper and downstream parts of the signal intersection(f_WZ)
#'
#' It follows <Table 8-22> in KHCM(2013)
#' @param construction_loc Upstream or downstream of the stop line. Choose one from : \code{'upstream'}, \code{'downstream'}
#' @param d_g During green time, the distance that the queue upstream of the stop line can be erased (m)
#' @param d_fb
#' @param N Number of lanes (number of stop-line lanes excluding left-turn pockets)
#' @param N_WZ The number of closed lanes under the road occupation construction (measured in the cross section of the construction section)
#' @param d_WZ Construction section separation distance (m)
#' @param L_WZ Construction section length (m)
#' @param g_T Total shock wave duration (s)
#' @param CRF Capacity Reduction Index
#' @param g_i Effective green time (s)
#' @param h_bar
#' @export f_WZ
#' @examples
f_WZ <- function(d_g = NULL, N = NULL, N_WZ = NULL, d_WZ = NULL, L_WZ = NULL, g_T = NULL, CRF = NULL, g_i = NULL){
  if (construction_loc == 'upstream'){
    if (d_g <= d_WZ){f <- 3600/h_bar - (3600 * N_WZ)/(g_i * N)}
    if (d_g > d_WZ){f <- (d_WZ * N - d_fb * N_WZ + CRF * (d_g - d_WZ + L_WZ * ((1 - CRF)/CRF)) * (N - N_WZ))/d_fb * (3600 / (g_i * N))}
  }
  if (construction_loc == 'downstream'){
    if (g_T >= g_i){f <- 3600 / h_bar}
    if (g_T < g_i){f <- (((g_T/h_bar * N)) + ((g_i - g_T)/h_bar) * CRF * (N - N_WZ)) * (3600 / (g_i * N))}
  }
  f
}
