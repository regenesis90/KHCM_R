#' Turning Traffic Volume Ratio of Shared Left-turning Lane Group i at Signalized Intersection
#'
#' Turn traffic ratio for shared left-turn lane group i.
#'     Ratio of left turns in the straight-left turn public lane group.
#'     This function follows <Formula 8-23> in KHCM(2013), p.242.
#' @param V_L Left Turn Traffic Volume(vph)
#' @param V_TH Straight-through traffic (vph)
#' @param V_RF Traffic going straight ahead of the first right turn on the shared right turn lane at the signal intersection(vph). See \code{\link{V_RF_si}}
#' @keywords Turning traffic volume ratio shared left-turning lane group signalized intersection
#' @seealso \code{\link{V_RF_si}}
#' @export P_LT_sh_si
#' @examples
#' P_LT_sh_si(V_L = 300, V_TH = 1000, V_RF = 283)
P_LT_sh_si <- function(V_L = NULL, V_TH = NULL, V_RF = NULL){
  if (V_L >= 0 & V_TH > 0 & V_RF > 0 & (V_TH - V_RF + V_L) > 0){
    p <- V_L / (V_TH - V_RF + V_L)
  }
  else {p <- 'Error : [V_L], [V_TH], [V_RF] must be positive(vph). And (V_TH - V_RF + V_L) > 0. Please check that.'}
  p
}
