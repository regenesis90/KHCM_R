#' The speed at which the end of the queue moves to the end of the internal link (mu_F, m/sec)
#'
#' TIt follows <Formula 9-5> in KHCM(2013), p.429
#' @param S_d_i Saturated traffic flow rate (= discharge traffic flow rate) of downstream intersection lane group i (car/sec)
#' @param mu_d_i Free passage speed of lane group i mitted from the downstream intersection (m/sec)
#' @param l_Q Average distance between vehicles in the queue (from the rear bumper of the vehicle in front to the rear bumper of the vehicle behind) (m)
#' @keywords
#' @export mu_F
#' @examples
mu_F <- function(S_d_i = NULL, mu_d_i = NULL, l_Q = NULL){
  S_d_i / ((S_d_i/mu_d_i) - (1/l_Q))
}
