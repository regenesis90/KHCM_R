#' Speed that End of the Queue Moves to the End of the Internal Link in Two-point Diamond-shaped Interchange
#'
#' Speed (m/s) at which the end of the queue moves to the end of the inner link in a two-point diamond-shaped interchange
#'     (highway connection road-general road junction)
#'     It follows <Formula 9-5> in KHCM(2013), p.430.
#' @param S_u_i Saturated traffic flow rate (= discharge traffic flow rate) of upstream intersection lane group i (car/sec)
#' @param mu_u_i Free passage speed of lane group i emitted from the upstream intersection (m/sec)
#' @param l_Q Average distance between vehicles in the queue (from the rear bumper of the vehicle in front to the rear bumper of the vehicle behind) (m)
#' @keywords speed queue two-point diamond interchange
#' @seealso \code{\link{tau_0_di}}, \code{\link{tau_1_di}}, \code{\link{mu_F_di}}
#' @export mu_B_di
#' @examples
#' mu_B_di(S_u_i = 2200, mu_u_i = 18.2, l_Q = 12.4)
mu_B_di <- function(S_u_i = NULL, mu_u_i = NULL, l_Q = NULL){
  if (S_u_i >= 0){
    if (mu_u_i > 0){
      if (l_Q > 0){
        m <- S_u_i / ((S_u_i/mu_u_i) - (1/l_Q))
      }
      else {m <- 'Error : [l_Q] must be positive(m). Please check that.'}
    }
    else {m <- 'Error : [mu_u_i] must be positive(m/s). Please check that.'}
  }
  else {m <- 'Error : [S_u_i] must be >= 0(veh/sec). Please check that.'}
  m
}
