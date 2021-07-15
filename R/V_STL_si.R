#' Straight-through Traffic Volume Using Shared Left-turn Lanes at Signalized Intersection
#'
#' Straight-through traffic volume(vph) using shared left-turn lanes at signalized intersection.
#'     This function follows <Formula 8-17>, <Formula 8-18> in KHCM(2013), p.240.
#' @param case Case of signalized intersection. Choose one from: \code{'case4'}, \code{'case5'}, \code{'case6'}. See \code{\link{case_si}}
#' @param V_R Right Turn Traffic Volume(vph). See \code{\link{V_R_si}}
#' @param V_L Left Turn Traffic Volume(vph)
#' @param V_TH Straight-through traffic (vph)
#' @param E_R Forward conversion factor for right turn. See \code{\link{E_R_si}}
#' @param E_L Forward conversion factor for left turn. See \code{\link{E_L_si}}
#' @param N Total number of access lanes (excluding dedicated left-turn lanes).
#' @keywords straight-through traffic volume public left-turn signalized intersection
#' @seealso \code{\link{E_R_si}}, \code{\link{E_L_si}}, \code{\link{lane_group_si}}, \code{\link{V_R_si}}
#' @export V_STL_si
#' @examples
#' V_STL_si(case = 'case6', V_R = 323, V_L = 291, V_TH = 999, E_R = 1.2, E_L = 1.09, N = 4)
V_STL_si <- function(case = NULL, V_R = NULL, V_L = NULL, V_TH = NULL, E_R = NULL, E_L = NULL, N = NULL){
  if (V_R >= 0 & V_L >= 0 & V_TH >= 0){
    if (E_R > 0 & E_L > 0){
      if (N >= 2){
        if (case == 'case4' | case == 'case6'){vstl <- (1/N) * (V_TH + E_R * V_R - E_L * V_L * (N - 1))}
        else if (case == 'case5'){vstl <- (1/N) * (2 * (V_TH + E_R * V_R) - E_L * V_L * (N - 2))}
        else {vstl <- 'Error : [case] must be one of [case4], [case5], [case6]. Please check that.'}
      }
      else {vstl <- 'Error : [N] must be >= 2 and integer. Please check that.'}
    }
    else {vstl <- 'Error : [E_R], [E_L] must be positive. Please check that.'}
  }
  else {vstl <- 'Error : [V_R], [V_L], [V_TH] must be >= 0(vph). Please check that.'}
  vstl
}
