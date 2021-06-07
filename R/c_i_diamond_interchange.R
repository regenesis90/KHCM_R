#' Capacity of the Diamond Interchange(c_i_diamond_interchange, vph)
#'
#' It follows <Formula 9-9> in KHCM(2013) p.433
#' @param S_i Saturated traffic flow rate (vph) for the i lane group
#' @param g_i_backtick Effective green time (seconds) for the i lane group reflecting the additional green loss time
#' @param C signal period(seconds)
#' @export c_i capacity of the i-th lane group
#' @examples
c_i_diamond_interchange <- function(S_i = NULL, g_i_backtick = NULL, C = NULL){
  S_i * g_i_backtick / C
}
