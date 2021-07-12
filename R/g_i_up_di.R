#' Corrected Effective Green Time at the Upstream Intersection Reflecting Additional Green Loss Time due to the Waiting Vehicle of the Inner Link in Two-point Diamond-shaped Interchange
#'
#' The corrected effective green time (seconds) of the upstream intersection i-lane group
#'     reflecting the loss time due to the waiting vehicle of the inner link
#'     in two-point diamond-shaped interchange.
#'    It follows <Formula 9-7> in KHCM(2013), p.432.
#' @param G Green time (seconds)
#' @param L_B Departure loss time (=2.3 seconds)
#' @param G_L Progress extension time (=2.0 seconds)
#' @param L_Q Additional green loss time (sec) due to the initial queue of the internal link
#' @details
#'     * Default value of Progress extention time(G_L) = 2.0(sec)
#'     * Default value of Departure loss time(L_B) = 2.3(sec)
#' @seealso \code{\link{g_i_dn_di}}, \code{\link{X_i_di}}, \code{\link{capa_i_di}}
#' @keywords corrected effective green time upstream additional green loss diamond interchange
#' @export g_i_up_di Corrected effective green time for upstream intersection i lane group (seconds)
#' @examples
#' g_i_up_di(G = 30, L_B = 2.1, G_L = 2.34, L_Q = 4)
#' g_i_up_di(G = 40, L_Q = 22)
g_i_up_di <- function(G = NULL, L_B = NULL, G_L = NULL, L_Q = NULL){
  if (G > 0 & L_Q > 0){
    if (is.null(L_B) == FALSE & is.null(G_L) == FALSE){
      if (L_B > 0 & G_L > 0){g <- G - L_B + G_L - L_Q}
      else {g <- 'Error : [L_B], [G_L] must be positive(sec). Please check that.'}
    }
    else {g <- G - 2.3 + 2.0 - L_Q}
  }
  else {g <- 'Error : [G], [L_Q] must be positive(sec). Please check that.'}
  g
}
