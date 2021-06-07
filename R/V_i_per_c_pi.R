#' Capacity ratio according to demand(V_i_per_c_pi)
#'
#' This function follows <Formula 10-2> in KHCM(2013) p.468
#' @param V_i Traffic Volume of traffic flow i
#' @param c_pi Potential capacity for moving flow i
#' @keywords
#' @export V_i_per_c_pi
#' @examples
V_i_per_c_pi <- function(V_i = NULL, c_pi = NULL){
  V_i / c_pi
}
