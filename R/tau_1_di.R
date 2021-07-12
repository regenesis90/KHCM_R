#' Time Taken from t0 Until the Queue Reaches the End of the Internal Link in Two-point Diamond-shaped Interchange
#'
#' In a two-point intersection diamond-shaped interchange (highway connection road-general road junction),
#'     the time taken from t0 until the queue reaches the end of the internal link
#'     It follows <Formula 9-3> in KHCM(2013), p.430.
#' @param Q_L Initial queue length (m) by appearance of internal link between intersections. Q_V * l_Q
#' @param D Length of internal link between intersections (m)
#' @param mu_B The speed at which the end of the queue moves to the end of the internal link (m/sec)
#' @keywords time queue internal link two-point diamond interchange
#' @seealso \code{\link{tau_0_di}}, \code{\link{tau_1_di}}, \code{\link{tau_2_di}}
#' @export tau_1_di
#' @examples
#' tau_1_di(Q_L = 320, D = 34.2, mu_B = 13.4)
tau_1_di <- function(Q_L = NULL, D = NULL, mu_B = NULL){
  if (mu_B > 0){
    if (D > 0){
      if (Q_L > 0){t <- (D - Q_L) / (-1 * mu_B)}
      else {t <- 'Error : [Q_L] must be positive(m). Please check that.'}
    }
    else {t <- 'Error : [D] must be positive(m). Please check that.'}
  }
  else {t <- 'Error : [mu_B] must be positive(m/s). Please check that.'}
  t
}
