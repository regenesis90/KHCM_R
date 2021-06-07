#' Time (in seconds) from t_1  when the queue arrives at the end of the internal link(tau_1)
#'
#' TIt follows <Formula 9-3> in KHCM(2013), p.429
#' @param Q_L Initial queue length (m) by appearance of internal link between intersections. Q_V * l_Q
#' @param D Length of internal link between intersections (m)
#' @param mu_B The speed at which the end of the queue moves to the end of the internal link (m/sec)
#' @keywords
#' @export tau_0
#' @examples
tau_1 <- function(D = NULL, Q_L = NULL, mu_B = NULL){
  (D - Q_L) / (-1 * mu_B)
}
