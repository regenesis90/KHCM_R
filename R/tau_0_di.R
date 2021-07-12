#' Time from the Start Until the First Vehicle Emitted From the Upstream Intersection Arrives at the End of the Queue in Two-point Diamond-shaped Interchange
#'
#' Time (in seconds) from the start until the first vehicle emitted from the upstream intersection arrives at the end of the queue
#'     in of the two-point diamond-shaped interchange (highway connection road-general road junction).
#'     It follows <Formula 9-2> in KHCM(2013), p.430.
#' @param Q_L Initial queue length (m) by appearance of internal link between intersections. Q_V * l_Q
#' @param D Length of internal link between intersections (m)
#' @param mu_u_i Free passage speed of lane group i emitted from the upstream intersection (m/sec)
#' @keywords
#' @seealso \code{\link{tau_2_di}}, \code{\link{L_Q_di}}
#' @export tau_0_di
#' @examples
#' tau_0_di(Q_L = 40, D = 100, mu_u_i = 18.2)
tau_0_di <- function(Q_L = NULL, D = NULL, mu_u_i = NULL){
  if (mu_u_i > 0){
    if (D > 0){
      if (Q_L > 0){t <- (D - Q_L) / mu_u_i}
      else {t <- 'Error : [Q_L] must be positive(m). Please check that.'}
    }
    else {t <- 'Error : [D] must be positive(m). Please check that.'}
  }
  else {t <- 'Error : [mu_u_i] must be positive(m/s). Please check that.'}
  t
}
