#' Corrected effective green time at the downstream intersection reflecting the additional green loss time in the downstream part due to non-use of green time(g_i_backtick_downstream)
#'
#' It follows <Formula 9-8> in KHCM(2013), p.433
#' @param G Green time (seconds)
#' @param L_DS Additional green loss time due to non-use of green time (seconds)
#' @param G_L Progress extension time (=2.0 seconds)
#' @param L_Q Additional green loss time (sec) due to the initial queue of the internal link
#' @export g_i_backtick_downstream Calibrated effective green time (seconds) for downstream intersection i-lane group
#' @examples
g_i_backtick_downstream <- function(G = NULL, L_B = NULL, G_L = NULL, L_Q = NULL){
  G - L_B + G_L - L_Q
}
