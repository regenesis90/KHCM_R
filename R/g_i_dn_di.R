#' Corrected Effective Green Time at the Downstream Intersection Reflecting Additional Green Loss Time due to the Waiting Vehicle of the Inner Link in Two-point Diamond-shaped Interchange
#'
#' The corrected effective green time (seconds) of the downstream intersection i-lane group
#'     reflecting the loss time due to the waiting vehicle of the inner link
#'     in two-point diamond-shaped interchange.
#'    It follows <Formula 9-7> in KHCM(2013), p.432.
#' @param G Green time (seconds)
#' @param G_L Progress extension time (=2.0 seconds)
#' @param L_B Departure loss time (=2.3 seconds)
#' @param L_DS Additional green loss time due to non-use of green time (seconds)
#' @details
#'     * Default value of Progress extention time(G_L) = 2.0(sec)
#'     * Default value of Departure loss time(L_B) = 2.3(sec)
#' @seealso \code{\link{g_i_up_di}}, \code{\link{X_i_di}}, \code{\link{capa_i_di}}
#' @keywords corrected effective green time downstream additional green loss diamond interchange
#' @export g_i_dn_di Calibrated effective green time (seconds) for downstream intersection i-lane group
#' @examples
#' g_i_dn_di(G = 30, L_B = 2.1, G_L = 2.34, L_DS = 4)
#' g_i_dn_di(G = 40, L_DS = 22)
g_i_dn_di <- function(G = NULL, G_L = 2.0, L_B = 2.3, L_DS = NULL){
  if (G > 0 & L_DS > 0){
    if (is.null(L_B) == FALSE & is.null(G_L) == FALSE){
      if (L_B > 0 & G_L > 0){g <- G - L_B + G_L - L_DS}
      else {g <- 'Error : [L_B], [G_L] must be positive(sec). Please check that.'}
    }
    else {g <- G - 2.3 + 2.0 - L_DS}
  }
  else {g <- 'Error : [G], [L_DS] must be positive(sec). Please check that.'}
  g
}
