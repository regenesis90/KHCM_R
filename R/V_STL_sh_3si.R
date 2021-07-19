#' Straight-through Traffic Using a Shared Left-turn Lane on Access Road with Only Straight and Left-turn Shared Lanes at 3-way Signalized Intersection
#'
#' On an access road with only straight and left turns at a three-way signal intersection,
#'     if there is a public lane for turning left,
#'     the amount of straight-through traffic arriving before the first left turn
#'     This function follows <Formula 8-34> in KHCM(2013), p.245.
#' @param V_L Left Turn Traffic Volume(vph)
#' @param V_TH Straight-through traffic (vph)
#' @param E_L Forward conversion factor for left turn. See \code{\link{E_L_si}}
#' @param N Total number of access lanes (excluding dedicated left-turn lanes).
#' @param L_H Loss of saturation headway time due to roadside friction on right-turn lanes at signal intersections. See \code{\link{L_H_si}}
#' @keywords straight-through traffic volume public left-turn signalized intersection
#' @seealso \code{\link{E_L_si}}, \code{\link{lane_group_3si}}, \code{\link{L_H_si}}
#' @export V_STL_sh_3si
#' @examples
#' V_STL_sh_3si(V_L = 291, V_TH = 999, E_L = 1.09, N = 4, L_H = 2.2)
V_STL_sh_3si <- function(V_L = NULL, V_TH = NULL, E_L = NULL, N = NULL, L_H = NULL){
  if (V_L >= 0 & V_TH >= 0){
    if (E_L > 0){
      if (N >= 1){
        if (L_H >= 0){vstl <- 1/N * (V_TH - E_L * V_L * (N - 1) + L_H/1.63)}
        else {vstl <- 'Error: [L_H] must be positive. Please check that.'}
      }
      else {vstl <- 'Error : [N] must be >= 1 and integer. Please check that.'}
    }
    else {vstl <- 'Error : [E_L] must be positive. Please check that.'}
  }
  else {vstl <- 'Error : [V_L], [V_TH] must be >= 0(vph). Please check that.'}
  vstl
}
