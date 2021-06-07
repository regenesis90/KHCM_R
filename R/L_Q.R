#' Additional green loss time at the upstream intersection due to the queuing of the internal link(L_Q)
#'
#' TIt follows <Formula 9-1> in KHCM(2013), p.429
#' @param Q_L Initial queue length (m) by appearance of internal link between intersections. Q_V * l_Q
#' @param mu_F The speed at which the front of the queue moves to the end of the internal link (m/sec)
#' @param mu_B The speed at which the end of the queue moves to the end of the internal link (m/sec)
#' @param tau_0 Time (in seconds) from the start until the first vehicle emitted from the upstream intersection arrives at the end of the queue
#' @param tau_1 Time (in seconds) from t_0 until the queue arrives at the end of the internal link
#' @keywords
#' @export L_Q it is same as tau_2. Time (in seconds) from t_1 to the time the queue is full
#' @examples
L_Q <- function(Q_L = NULL, mu_F = NULL, mu_B = NULL, tau_0 = NULL, tau_1 = NULL){
  (Q_L + mu_F * tau_0 + (mu_F - mu_B) * tau_1)/(-1 * mu_F)
}
