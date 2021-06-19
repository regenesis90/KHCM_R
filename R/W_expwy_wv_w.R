#' Weaving Strength Factor According to Weaving Traffic Flow
#'
#' This function calculates the weaving strength factor according to weaving traffic flow.
#'     It follows <Formula 3-2> in KHCM(2013), p.61.
#' @param V Total traffic flow in expressway weaving section(pcph).
#' @param V_w Weaving traffic flow(pcph).
#' @param N Total number of lanes in the weaving section.
#' @param L Length of the weaving section(m).
#' @keywords weaving strength factor weaving section expressway
#' @seealso \code{\link{VR_expwy_wv}}, \code{\link{W_expwy_wv_nw}}, \code{\link{S_expwy_wv}}
#' @export W_expwy_wv_w \code{0.059 * (1 + VR)**(2.2) * (V/N)**(0.97) / (L**(0.80))}
#' @examples
#' W_expwy_wv_w(V_w = 321, V = 1892, N = 3, L = 240)
W_expwy_wv_w <- function(V = NULL, V_w = NULL, N = NULL, L = NULL){
  VR <- VR_expwy_wv(V_w = V_w, V = V)
  if (VR >= 0 & VR <= 1){
    if (N > 0){
      if (L > 0){0.059 * (1 + VR)**(2.2) * (V/N)**(0.97) / (L**(0.80))}
      else {'Error : [L] must be positive(m). Please check that.'}
    }
    else{'Error : [N] must be positive. Please check that.'}
  }
  else {'Error : [VR] must be <= 1 and >= 0. Please check that. See [VR_expwy_wv()].'}
}
