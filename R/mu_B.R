#' The speed at which the end of the queue moves to the end of the internal link (mu_B, m/sec)
#'
#' TIt follows <Formula 9-3> in KHCM(2013), p.429
#' @param S_u_i Saturated traffic flow rate (= discharge traffic flow rate) of upstream intersection lane group i (car/sec)
#' @param mu_u_i Free passage speed of lane group i mitted from the upstream intersection (m/sec)
#' @param l_Q Average distance between vehicles in the queue (from the rear bumper of the vehicle in front to the rear bumper of the vehicle behind) (m)
#' @keywords
#' @export mu_B
#' @examples
mu_B <- function(S_u_i = NULL, mu_u_i = NULL, l_Q = NULL){
  S_u_i / ((S_u_i/mu_u_i) - (1/l_Q))
}
