#' Capacity of Lane Group i at Signalized Intersection
#'
#' Capacity of lane group i at signalized intersection.
#'     It follows <Formula 8-40> in KHCM(2013), p.250.
#' @param S_i Saturated traffic flow rate (vph) for the i lane group. See \code{\link{S_i_si}}
#' @param g_i Effective green time for i lane group (seconds)
#' @param C Signal period(seconds)
#' @seealso \code{\link{S_i_si}}, \code{\link{X_i_si}}
#' @export capa_i_si capacity of the i-th lane group
#' @examples
#' capa_i_si(S_i = 1234, g_i = 80, C = 240)
capa_i_si <- function(S_i = NULL, g_i = NULL, C = NULL){
  if (S_i >= 0){
    if (g_i > 0 & C > 0){c <- S_i * g_i / C}
    else {c <- 'Error : [g_i], [C] must be positive(sec). Please check that.'}
  }
  else {c <- 'Error : [S_i] must be positive(vph). Please check that.'}
  c
}
