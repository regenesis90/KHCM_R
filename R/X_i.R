#' Saturation of lane group i(X_i)
#'
#' It follows <Formula 8-41> in KHCM(2013)
#' @param S_i Saturated traffic flow rate (vph) for the i lane group
#' @param g_i Effective green time for i lane group (seconds)
#' @param C signal period(seconds)
#' @param V_i i traffic volume in lane group (vph)
#' @export X_i capacity of the i-th lane group
#' @examples
X_i <- function(S_i = NULL, g_i = NULL, C = NULL, V_i = NULL){
  V_i * C / (S_i * g_i)
}
