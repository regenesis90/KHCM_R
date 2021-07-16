#' Straight-through Traffic Volume Using Shared Right-turn Lanes at 3-way Signalized Intersection
#'
#' Straight-through traffic volume(vph) using shared right-turn lanes at 3-way signalized intersection.
#'     This function follows <Formula 8-19> in KHCM(2013), p.240, 244.
#' @param V_R Right Turn Traffic Volume(vph)
#' @param V_TH Straight-through traffic (vph)
#' @param E_R Forward conversion factor for right turn
#' @param N Total number of access lanes (excluding dedicated left-turn lanes).
#' @keywords straight-through traffic volume public left-turn signalized intersection
#' @seealso \code{\link{E_R_si}}, \code{\link{lane_group_3si}}
#' @export V_STR_sh_3si
#' @examples
#' V_STR_sh_3si(V_R = 92, V_TH = 763, E_R = 1.3, N = 5)
V_STR_sh_3si <- function(V_R = NULL, V_TH = NULL, E_R = NULL, N = NULL){
  if (V_R >= 0 & V_TH >= 0){
    if (E_R > 0){
      if (N >= 1){vstr <- 1/N * (V_TH - E_R * V_R * (N - 1))}
      else {vstr <- 'Error : [N] must be >= 1 and integer. Please check that.'}
    }
    else {vstr <- 'Error : [E_R] must be positive. Please check that.'}
  }
  else {vstr <- 'Error : [V_R], [V_TH] must be >= 0(vph). Please check that.'}
  vstr
}
