#' Straight-through Traffic Volume Using Shared Right-turn Lanes at Signalized Intersection
#'
#' Straight-through traffic volume(vph) using shared right-turn lanes at signalized intersection.
#'     This function follows <Formula 8-19> in KHCM(2013), p.240, 244.
#' @param V_R Right Turn Traffic Volume(vph)
#' @param V_L Left Turn Traffic Volume(vph)
#' @param V_TH Straight-through traffic (vph)
#' @param E_R Forward conversion factor for right turn
#' @param E_L Forward conversion factor for left turn
#' @param N Total number of access lanes (excluding dedicated left-turn lanes).
#' @keywords straight-through traffic volume public left-turn signalized intersection
#' @seealso \code{\link{E_R_si}}, \code{\link{E_L_si}}, \code{\link{lane_group_si}}
#' @export V_STR_si
#' @examples
#' V_STR_si(case = 'case4', V_R = 92, V_L = 103, V_TH = 763, E_R = 1.3, E_L = 2.123, N = 5)
V_STR_si <- function(case = NULL, V_R = NULL, V_L = NULL, V_TH = NULL, E_R = NULL, E_L = NULL, N = NULL){
  if (V_R >= 0 & V_L >= 0 & V_TH >= 0){
    if (E_R > 0 & E_L > 0){
      if (N >= 2){
        if (case == 'case1' | case == 'case2' | case == 'case3'){vstr <- (1/N) * (V_TH + E_R * V_R * (N - 1))}
        else if (case == 'case4' | case == 'case5' | case == 'case6'){vstr <- (1/N) * (V_TH + E_L * V_L - E_R * V_R * (N - 1))}
        else {vstr <- 'Error : [case] must be one of [case4], [case5], [case6]. Please check that.'}
      }
      else {vstr <- 'Error : [N] must be >= 2 and integer. Please check that.'}
    }
    else {vstr <- 'Error : [E_R], [E_L] must be positive. Please check that.'}
  }
  else {vstr <- 'Error : [V_R], [V_L], [V_TH] must be >= 0(vph). Please check that.'}
  vstr
}
