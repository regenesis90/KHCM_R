#' Additional Green Loss Time at the Upstream Intersection due to the Queuing of the Internal Link at Two-point Diamond-shaped Interchange
#'
#' In the two-point intersection diamond-shaped interchange (highway connection road-general road junction),
#'     the additional green loss time at the upstream intersection due to the queuing of the internal link.
#'     It follows <Formula 9-1> in KHCM(2013), p.429.
#' @param Q_L Initial queue length (m) by appearance of internal link between intersections. Q_V * l_Q
#' @param l_Q Average distance between vehicles in the queue (from the rear bumper of the vehicle)
#' @param S_u_i Saturated traffic flow rate (= discharge traffic flow rate) of upstream intersection lane group i (car/sec)
#' @param S_d_i Saturated traffic flow rate (= discharge traffic flow rate) of downstream intersection lane group i (car/sec)
#' @param mu_u_i Free passage speed of lane group i emitted from the upstream intersection (m/s)
#' @param mu_d_i Free passage speed of lane group i emitted from the downstream intersection (m/s)
#' @param D Length of internal link between intersections(m).
#' @keywords Green loss time upstream intersection queue two-point diamond interchange
#' @seealso \code{\link{tau_0_di}}, \code{\link{tau_1_di}}, \code{\link{mu_F_di}}, \code{\link{mu_B_di}}
#' @export tau_2_di
#' @examples
#' tau_2_di(Q_L = 264.3, l_Q = 14, S_u_i = 1400, S_d_i = 2200, mu_u_i = 22.3, mu_d_i = 20.1, D = 10)
tau_2_di <- function(Q_L = NULL, l_Q = NULL, S_u_i = NULL, S_d_i = NULL, mu_u_i = NULL, mu_d_i = NULL, D = NULL){
  if (Q_L > 0){
    if (l_Q > 0){
      if (D > 0){
        mu_F <- mu_F_di(S_d_i = S_d_i, mu_d_i = mu_d_i, l_Q = l_Q)
        mu_B <- mu_B_di(S_u_i = S_u_i, mu_u_i = mu_u_i, l_Q = l_Q)
        tau_0 <- tau_0_di(D = D, Q_L = Q_L, mu_u_i = mu_u_i)
        tau_1 <- tau_1_di(D = D, Q_L = Q_L, mu_B = mu_B)
        if (is.numeric(mu_F) == TRUE){
          if (is.numeric(mu_B) == TRUE){
            if (is.numeric(tau_0) == TRUE){
              if (is.numeric(tau_1) == TRUE){
                t <- (Q_L + mu_F * tau_0 + (mu_F - mu_B) * tau_1)/(-1 * mu_F)
              }
              else {t <- tau_1}
            }
            else {t <- tau_0}
          }
          else {t <- mu_B}
        }
        else {t <- mu_F}
      }
      else {t <- 'Error : [D] must be positive(m). Please check that.'}
    }
    else {t <- 'Error : [l_Q] must be positive(m). Please check that.'}
  }
  else {t <- 'Error : [Q] must be positive(m). Please check that.'}
  t
}
