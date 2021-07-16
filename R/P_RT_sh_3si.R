#' Turning Traffic Volume Ratio of Shared Right-turning Lane Group i at 3-way Signalized Intersection
#'
#' Turn traffic ratio for shared right-turn lane group i at 3-way signalized intersection
#'     Ratio of right turns in the straight-right turn shared lane group.
#'     In the case of the direct-right shared lane group,
#'     the turning traffic volume ratio of the right-turning lane group on an access road with only straight and right turns at the 3-way signalized intersection.
#'     Proportion of right turns in the straight-right turn shared lane group.
#'     This function follows <Formula 8-24> in KHCM(2013), p.242, 244.
#' @param V_R Right Turn Traffic Volume(vph)
#' @param V_TH Straight-through traffic (vph)
#' @param V_LF Traffic going straight ahead of the first left turn on the shared left turn lane at the signal intersection(vph). See \code{\link{V_RF_si}}
#' @keywords Turning traffic volume ratio shared right-turning lane group 3-way signalized intersection
#' @seealso \code{\link{V_RF_si}}
#' @export P_RT_sh_3si
#' @examples
#' P_RT_sh_3si(V_R = 300, V_TH = 1000, V_LF = 283)
P_RT_sh_3si <- function(V_R = NULL, V_TH = NULL, V_LF = NULL){
  if (V_R >= 0 & V_TH > 0 & V_LF > 0 & (V_TH - V_LF + V_R) > 0){
    p <- V_R / (V_TH - V_LF + V_R)
  }
  else {p <- 'Error : [V_R], [V_TH], [V_LF] must be positive(vph). And (V_TH - V_LF + V_R) > 0. Please check that.'}
  p
}
