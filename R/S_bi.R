#' Saturated traffic flow rate (vphg) of lane group i in the exclusive median bus lane(S_bi, vphg)
#'
#' It follows <Formula 8-58> in KHCM(2013)
#' @param N_i the number of lanes in the i lane group
#' @param f_BLT Left turn correction factor on exclusive median bus lane (1.0 in case of going straight)
#' @param f_g Approach slope correction factor
#' @param f_ub Upstream stop influence correction factor
#' @export S_bi Saturated traffic flow rate (vphg) of lane group i in the exclusive median bus lane
#' @examples
S_bi <- function(N_i = NULL, f_BLT = NULL, f_g = NULL, f_ub = NULL){
  1100 * N_i * f_BLT * f_g * f_ub
}
