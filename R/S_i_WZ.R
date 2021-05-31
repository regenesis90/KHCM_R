#' Correction of saturated traffic flow rate due to road occupation construction near intersections(S_i_WZ)
#'
#' It follows <Formula 8-66> in KHCM(2013)
#' @param N_i number of lanes in the i lane group
#' @param f_RT
#' @param f_w3 Lane width correction factor
#' @param f_g Approach slope correction factor
#' @param f_hv3 Heavy vehicle factor
#' @param f_WZ
#' @export S_i_WZ Saturated traffic flow rate lane group i due to occupancy construction near the intersection(vphg)
#' @examples
S_i_WZ <- function(N_i = NULL, f_LRT = NULL, f_w3 = NULL, f_g = NULL, f_hw3 = NULL, f_WZ = NULL){
  S_i(N_i = N_i, f_LRT = F_RT, f_w3 = f_w3, f_g = f_g, f_hw3 = f_hw3) * f_WZ
}
