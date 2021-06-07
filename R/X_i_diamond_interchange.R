#' Saturation of the i lane group(X_i_diamond_interchange)
#'
#' It follows <Formula 9-10> in KHCM(2013) p.434
#' @param V_i Traffic volume of i lane group(vph)
#' @param S_i Saturated traffic flow rate (vph) for i lane group
#' @param g_i_backtick Effective green time (seconds) for the i lane group reflecting the additional green loss time
#' @param C period(seconds)
#' @export X_i \code{(V/c)_i = V_i/(S_i * (g_i_backtick/C))}
#' @examples
X_i_diamond_interchange <- function(V_i = NULL, S_i = NULL, g_i_backtick = NULL, C = NULL){
  V_i * C / (S_i * g_i_backtick)
}
