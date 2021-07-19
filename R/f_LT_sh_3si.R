#' Saturated Traffic Volume Correction Factor for Straight-left Shared Lane Group at 3-way Signalized Intersection
#'
#' Saturated traffic volume correction factor for straight-left shared lane group at three-way signalized intersection.
#'     This function follows <Formula 8-36> in KHCM(2013), p.246.
#' @param E_L Forward conversion coefficient for public Left turns in a left turn lane at signalized intersection.
#' @param P_LT Turn traffic ratio for shared Left-turn lane group i.
#' @param L_H Friction by roadside lane going straight (seconds). See \code{\link{L_H_si}}
#' @param V_L Left Turn Traffic Volume(vph)
#' @keywords Left turn correction factor shared lane 3-way signalized intersection
#' @seealso \code{\link{E_L_si}}, \code{\link{P_LT_sh_si}}
#' @export f_LT_sh_3si Left turn correction factor
#' @examples
#' f_LT_sh_3si(E_L = 1.32, P_LT = 0.33, L_H = 3.3, V_L = 123)
f_LT_sh_3si <- function(E_L = NULL, P_LT = NULL, L_H = NULL, V_L = NULL){
  if (is.numeric(E_L) == TRUE & P_LT >= 0 & P_LT <= 1){
    if (V_L > 0){
      if (L_H > 0){fl <- 1 / (1 + P_LT * (E_L - 1 + (L_H / (1.63 * V_L))))}
      else {fl <- 'Error : [L_H] must be positive(sec). Please check that.'}
    }
    else {fl <- 'Error : [V_L] must be positive(vph). Please check that.'}
  }
  else {fl <- 'Error : [E_L] must be numeric. [P_LT] must be >= 0 and <= 1. Please check that.'}
  fl
}
