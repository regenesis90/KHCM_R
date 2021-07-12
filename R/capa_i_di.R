#' Capacity of i lane-group in Two-point Diamond-shaped Interchange
#'
#' The capacity of the i-lane group of upstream and downstream intersection access roads.
#'     It is classified by lane group according to each city.
#'     It follows <Formula 9-9> in KHCM(2013) p.433.
#' @param S_i Saturated traffic flow rate (vph) for the i lane group
#' @param g_i Effective green time (sec) for the i lane group reflecting the additional green loss time. See \code{\link{g_i_up_di}}, \code{\link{g_i_dn_di}}
#' @param C signal period(sec)
#' @keywords capacity two-point diamond interchange
#' @seealso \code{\link{g_i_up_di}}, \code{\link{g_i_dn_di}}, \code{\link{X_i_di}}
#' @export capa_i_di capacity of the i-th lane group
#' @examples
#' capa_i_di(S_i = 1200, g_i = 24, C = 120)
capa_i_di <- function(S_i = NULL, g_i = NULL, C = NULL){
  if (S_i > 0){
    if (g_i > 0 & C > 0){c <- S_i * g_i / C}
    else {c <- 'Error : [g_i], [C] must be positive(sec). Please check that.'}
  }
  else {c <- 'Error : [S_i] must be positive(vph). Please check that.'}
  c
}
