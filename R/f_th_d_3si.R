#' Correction Coefficient for Straight-only Lanes with Right-side Friction at 3-way Signalized Intersection
#'
#' If there is a left-turn exclusive lane on an approach with only straight and left turns at a three-way signal intersection,
#'     it is correction coefficient for straight-only lanes with right-side friction at 3-way signalized intersection
#'     This function follows <Formula 8-33> in KHCM(2013), p.245.
#' @param L_H Loss of saturation headway time due to roadside friction on right-turn lanes at signal intersections(sec). See \code{\link{L_H_si}}
#' @param N The total number of lanes on the approach road excluding left-turn lanes
#' @keywords correction coefficient straight-only lane right-side friction 3-way three-way signalized intersection
#' @seealso \code{\link{L_H_si}}
#' @export  right turn correction factor
#' @examples
#' f_th_d_3si(L_H = 3.3, N = 3)
f_th_d_3si <- function(L_H = NULL, N = NULL){
  if (L_H > 0){
    if (N >= 1){
      f <- 1 - L_H / (3600 * N)
    }
    else {f <- 'Error : [N] must be positive integer. Please check that.'}
  }
  else {f <- 'Error : [L_H] must be positive(sec). Please check that.'}
  f
}
