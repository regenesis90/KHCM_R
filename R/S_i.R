#' Saturated traffic flow rate of lane group i (S_ivphg)
#'
#' It follows <Formula 8-38>, <Formula 8-68> in KHCM(2013)
#' @param N_i number of lanes in the i lane group
#' @param f_LRT f_LT or f_RT
#' @param f_w3 Lane width correction factor
#' @param f_g Approach slope correction factor
#' @param f_hv3 Heavy vehicle factor
#' @param f_IW weather correction factor
#' @export S_i Saturated traffic flow rate of lane group i
#' @examples
S_i <- function(N_i = NULL, f_LRT = NULL, f_w3 = NULL, f_g = NULL, f_hw3 = NULL, f_IW = 1){
  2200 * N_i * f_LRT * f_w3 * f_g * f_hv3
}
