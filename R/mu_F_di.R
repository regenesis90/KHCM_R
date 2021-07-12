#' Speed that Front of the Queue Moves to the End of the Internal Link in Two-point Diamond-shaped Interchange
#'
#' Speed (m/s) at which the front of the queue moves to the end of the inner link in a two-point diamond-shaped interchange
#'     (highway connection road-general road junction)
#'     It follows <Formula 9-4> in KHCM(2013), p.430.
#' @param S_d_i Saturated traffic flow rate (= discharge traffic flow rate) of downstream intersection lane group i (car/sec)
#' @param mu_d_i Free passage speed of lane group i mitted from the downstream intersection (m/sec)
#' @param l_Q Average distance between vehicles in the queue (from the rear bumper of the vehicle in front to the rear bumper of the vehicle behind) (m)
#' @keywords Speed End Queue two-point diamond interchange
#' @seealso \code{\link{mu_B_di}}, \code{\link{tau_0_di}}, \code{\link{tau_1_di}}
#' @export mu_F_di
#' @examples
#' mu_F_di(S_d_i = 2200, mu_d_i = 20, l_Q = 10)
mu_F_di <- function(S_d_i = NULL, mu_d_i = NULL, l_Q = NULL){
  if (S_d_i >= 0){
    if (mu_d_i > 0){
      if (l_Q > 0){
        m <- S_d_i / ((S_d_i/mu_d_i) - (1/l_Q))
      }
      else {m <- 'Error : [l_Q] must be positive(m). Please check that.'}
    }
    else {m <- 'Error : [mu_d_i] must be positive(m/s). Please check that.'}
  }
  else {m <- 'Error : [S_d_i] must be positive(veh/sec). Please check that.'}
  m
}
