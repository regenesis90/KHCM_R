#' Saturation of the i lane group(X_i_diamond_interchange)
#'
#' The saturation of the i-lane group (ratio of traffic volume and capacity) in two-point diamond-shaped interchange.
#'     It follows <Formula 9-10> in KHCM(2013), p.434.
#' @param v_i Traffic volume of i lane group(vph)
#' @param S_i Saturated traffic flow rate (vph) for the i lane group
#' @param g_i Effective green time (sec) for the i lane group reflecting the additional green loss time. See \code{\link{g_i_up_di}}, \code{\link{g_i_dn_di}}
#' @param C signal period(sec)
#' @keywords Saturation flow two-point diamond interchange
#' @seealso \code{\link{g_i_up_di}}, \code{\link{g_i_dn_di}}, \code{\link{X_i_di}}
#' @export X_i_di \code{(V/c)_i = V_i/(S_i * (g_i_backtick/C))}
#' @examples
#' X_i_di(v_i = 323, S_i = 1300, g_i = 30, C = 120)
X_i_di <- function(v_i = NULL, S_i = NULL, g_i = NULL, C = NULL){
  if (v_i > 0){
    capa <- capa_i_di(S_i = S_i, g_i = g_i, C = C)
    if (is.numeric(capa) == TRUE){x <- v_i/capa}
    else {x <- capa}
  }
  else {x <- 'Error : [v_i] must be positive(vph). Please check that.'}
  x
}
