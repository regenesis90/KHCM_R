#' Corrected effective green time at the downstream intersection reflecting the additional green loss time in the downstream part due to non-use of green time(g_i_backtick_downstream)
#'
#' It follows <Formula 9-8> in KHCM(2013), p.433
#' @param G Green time (seconds)
#' @param G_L Progress extension time (=2.0 seconds)
#' @param L_B Departure loss time (=2.3 seconds)
#' @param L_DS Additional green loss time due to non-use of green time (seconds)
#' @export g_i_backtick_downstream Calibrated effective green time (seconds) for downstream intersection i-lane group
#' @examples
g_i_backtick_downstream <- function(G = NULL, G_L = NULL, L_B = NULL, L_DS = NULL){
  G - L_B + G_L - L_DS
}
