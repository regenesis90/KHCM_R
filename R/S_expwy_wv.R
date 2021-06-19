#' Total Average Speed in Expressway Weaving Section
#'
#' This function calculates the total average speed in expressway weaving section.(kph)
#'    It follows <Formula 3-4> in KHCM(2013), p.62.
#' @param design_speed Design speed of the main lane(kph). Choose one from:
#' @param V Total traffic flow in expressway weaving section(pcph).
#' @param V_w weaving traffic flow(pcph).
#' @param N Total number of lanes in the weaving section.
#' @param L Length of the weaving section(m).
#' @keywords average speed expressway weaving section
#' @seealso \code{\link{S_expwy_wv_w}}, \code{\link{S_expwy_wv_w}}, \code{\link{W_expwy_wv_w}}, \code{\link{W_expwy_wv_nw}}
#' @export S_expwy_wv \code{V/(V_w/S_w + (V_nw/S_nw))}
#' @examples
#' S_expwy_wv(design_speed = 100, V = 1283, V_w = 459, N = 3, L = 600)
S_expwy_wv <- function(design_speed = NULL, V = NULL, V_w = NULL, N = NULL, L = NULL){
  S_w <- S_expwy_wv_w(design_speed = design_speed, V = V, V_w = V_w, N = N, L = L)
  S_nw <- S_expwy_wv_nw(design_speed = design_speed, V = V, V_w = V_w, N = N, L = L)
  if (is.numeric(S_w) == TRUE){
    if (is.numeric(S_nw) == TRUE){V / ((V_w / S_w)+((V - V_w) / S_nw))}
    else {print(S_nw)}
  }
  else {print(S_w)}
}
