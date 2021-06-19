#' Weaving Strength Factor According to Non-Weaving Traffic Flow(W_nw)
#'
#' This function calculates the weaving strength factor according to non-weaving traffic flow.
#'     It follows <Formula 3-2> in KHCM(2013), p.61.
#' @param V Total traffic flow in expressway weaving section(pcph).
#' @param V_w Weaving traffic flow(pcph).
#' @param N Total number of lanes in the weaving section.
#' @param L Length of the weaving section(m).
#' @export W_expwy_wv_nw \code{0.00000054*(1 + VR)**(0.68)*(V/N)**(2.0)/(L**(0.17))}
#' @keywords weaving strength factor weaving section expressway
#' @seealso \code{\link{VR_expwy_wv}}, \code{\link{W_expwy_wv_w}}, \code{\link{S_expwy_wv}}
#' @examples
#' W_expwy_wv_nw(V = 2123, V_w = 832, N = 4, L = 550)
W_expwy_wv_nw <- function(V = NULL, V_w = NULL, N = NULL, L = NULL){
  VR <- VR_expwy_wv(V_w = V_w, V = V)
  if (VR >= 0 & VR <= 1){
    if (N > 0){
      if (L > 0){0.00000054 * (1 + VR)**(0.68) * (V/N)**(2.0) / (L**(0.17))}
      else {'Error : [L] must be positive(m). Please check that.'}
    }
    else{'Error : [N] must be positive. Please check that.'}
  }
  else {'Error : [VR] must be <= 1 and >= 0. Please check that. See [VR_expwy_wv()].'}
}
