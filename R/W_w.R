#' Weaving Strength Factor According to Weaving Traffic Flow(W_w)
#'
#' This function calculates the weaving strength factor according to weaving traffic flow(W_w)
#' @param V *Numeric* Total traffic volume (pcph) of the weaving section
#' @param V_w *Numeric* Weaving traffic volume(pcph)
#' @param N *Numeric* Total number of lanes in the weaving section
#' @param L *Numeric* Length of the weaving section(m)
#' @export W_w Weaving strength factor according to weaving traffic flow
#' @examples
#' W_w(V = 1000, V_w = 300, N = 1, L = 100)
#' W_w(1200, 100, 2, 200)
W_w <- function(V = NULL, V_w = NULL, N = NULL, L = NULL){
  if (V > 0 & V_w > 0 & N >= 1 & L > 0){
    VR <- V_w/V
    0.059 * (1 + VR)**(2.2) * (V/N)**(0.97) / (L**(0.80))
  }
}
