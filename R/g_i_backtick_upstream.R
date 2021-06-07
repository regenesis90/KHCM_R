#' Corrected effective green time at the upstream intersection reflecting the loss time due to the waiting vehicle of the internal link(g_i_backtick)
#'
#' It follows <Formula 9-7> in KHCM(2013), p.432
#' @param G Green time (seconds)
#' @param L_B Departure loss time (=2.3 seconds)
#' @param G_L Progress extension time (=2.0 seconds)
#' @param L_Q Additional green loss time (sec) due to the initial queue of the internal link
#' @export g_i_backtick_upstream Corrected effective green time for upstream intersection i lane group (seconds)
#' @examples
g_i_backtick_upstream <- function(G = NULL, L_B = NULL, G_L = NULL, L_Q = NULL){
  G - L_B + G_L - L_Q
}
