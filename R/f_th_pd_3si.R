#' Correction Coefficient for Practically Dedicated Lanes with Right-side Friction at 3-way Signalized Intersection
#'
#' When an actual exclusive lane group is formed at a three-way signal intersection,
#'     the remaining lane groups become a straight-forward exclusive lane group.
#'     The correction factor for this straight-only lane group.
#'     This function follows <Formula 8-35> in KHCM(2013), p.245.
#' @param L_H Loss of saturation headway time due to roadside friction on right-turn lanes at signal intersections(sec). See \code{\link{L_H_si}}
#' @param N_th Number of lanes dedicated to going straight excluding lanes for practically dedicated left turns
#' @keywords correction coefficient straight-only lane right-side friction 3-way three-way signalized intersection
#' @export f_th_pd_3si
#' @examples
#' f_th_pd_3si(L_H = 3.3, N_th = 3)
f_th_pd_3si <- function(L_H = NULL, N_th = NULL){
  if (L_H > 0){
    if (N_th >= 1){
      f <- 1 - L_H / (3600 * N_th)
    }
    else {f <- 'Error : [N_th] must be positive integer. Please check that.'}
  }
  else {f <- 'Error : [L_H] must be positive(sec). Please check that.'}
  f
}
