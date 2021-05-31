#' Capacity of the i-th lane group(c_i, vph)
#'
#' It follows <Formula 8-40> in KHCM(2013)
#' @param S_i Saturated traffic flow rate (vph) for the i lane group
#' @param g_i Effective green time for i lane group (seconds)
#' @param C signal period(seconds)
#' @export c_i capacity of the i-th lane group
#' @examples
c_i <- function(S_i = NULL, g_i = NULL, C = NULL){
  S_i * g_i / C
}
