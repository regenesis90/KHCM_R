#' Green time erasable distance (d_g, m)
#'
#' It follows <Formula 8-59> in KHCM(2013)
#' @param d_fb Headspace spacing when the vehicle is stopped (m)
#' @param g_i Effective green time (s)
#' @param h_bar Saturation difference time (s)
#' @export d_g Green time erasable distance
#' @examples
d_g <- function(d_fb = NULL, g_i = NULL, h_bar = NULL){
  g_i / h_bar * d_fb
}
