#' Time (in seconds) from the start until the first vehicle emitted from the upstream intersection arrives at the end of the queue(tau_0)
#'
#' TIt follows <Formula 9-2> in KHCM(2013), p.429
#' @param Q_L Initial queue length (m) by appearance of internal link between intersections. Q_V * l_Q
#' @param D Length of internal link between intersections (m)
#' @param mu_u_i Free passage speed of lane group i mitted from the upstream intersection (m/sec)
#' @keywords
#' @export tau_0
#' @examples
tau_0 <- function(D = NULL, Q_L = NULL, mu_u_i = NULL){
  (D - Q_L) / mu_u_i
}
