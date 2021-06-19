#' Average Density in Freeway Weaving Section(D_avg_weaving, pcpkmpl)
#'
#' This function calculates the average density in freeway weaving section.(pcpkmpl)
#'    It follows <Formula 3-5> in KHCM(2013), p.62.
#' @param design_speed Design speed of the main lane(kph). Choose one from:
#' @param V Total traffic flow in expressway weaving section(pcph). See \code{\link{V_P_expwy_wv}}
#' @param V_w weaving traffic flow(pcph).
#' @param N Total number of lanes in the weaving section.
#' @param L Length of the weaving section(m).
#' @keywords average debnsity expressway weaving section
#' @seealso \code{\link{S_expwy_wv}}
#' @export D_expwy_wv
#' @examples
#' D_expwy_wv(design_speed = 100, V = 1283, V_w = 459, N = 3, L = 600)
D_expwy_wv <- function(design_speed = NULL, V = NULL, V_w = NULL, N = NULL, L = NULL){
  S <- S_expwy_wv(design_speed = design_speed, V = V, V_w = V_w, N = N, L = L)
  if (is.numeric(S) == TRUE){
    (V / N) / S
  }
  else{print(S)}
}
