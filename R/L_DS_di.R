#' Additional Green Loss Time in the Downstream Part due to Non-use of Green Time in Two-point Diamond-shaped Interchange
#'
#' Additional green loss time in the downstream part due to non-use of green time
#'     in the two-point intersection diamond-shaped interchange (highway connection road-general road joint part).
#'     It follows <Formula 9-6> in KHCM(2013), p.432.
#' @param CG_DS Period in which the upstream intersection shows a left turn and the downstream intersection shows green(sec)
#' @param Q_V Average number of waiting vehicles in the internal link at the start of each signal (veh)
#' @param h_s Saturation difference time (sec/veh)
#' @keywords additional green loss time downstream non-use two-point diamond interchange
#' @seealso \code{\link{g_i_dn_di}}
#' @export L_DS_di
#' @examples
#' L_DS_di(CG_DS = 30, Q_V = 10, h_s = 2)
L_DS_di <- function(CG_DS = NULL, Q_V = NULL, h_s = NULL){
  if (CG_DS > 0){
    if (Q_V >= 0){
      if (h_s >= 0){l <- CG_DS - Q_V * h_s}
      else {l <- 'Error : [h_s] must be >= 0. Please check that.'}
    }
    else {l <- 'Error : [Q_V] must be >= 0. Please check that.'}
  }
  else {l <- 'Error : [CG_DS] must be positive(s). Please check that.'}
  l
}
