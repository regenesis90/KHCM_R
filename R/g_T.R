#' Total shock wave session time(g_T)
#'
#' It follows <Formula 8-60>, <Formula 8-61> in KHCM(2013)
#' @param d_T Construction section taper length (m)
#' @param d_WZ Construction section separation distance (m)
#' @param d_IW Intersection width (m)
#' @param u_o Average moving speed(m/s)
#' @param w Shock wave speed(m/s)
#' @export g_T Total shock wave session time(g_T)
#' @examples
g_T <- function(d_T = NULL, d_WZ = NULL, d_IW = NULL, u_o = NULL, w = NULL){
  g_F <- (d_T + d_WZ + d_IW)/u_o
  g_B <- (d_WZ + 0.5 * d_IW)/w
  if (g_F >= 0 & g_B >= 0){
    g_T <- g_F + g_B + 1.0
    g_T
  }
}
