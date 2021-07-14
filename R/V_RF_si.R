#' Traffic Going Straight Ahead of the First Right Turn on the Public Right Turn Lane at Signalized Intersection
#'
#' Traffic going straight ahead of the first right turn on the public right turn lane at the signal intersection.
#'     This function follows <Formula 8-15>, <Formula 8-16> in KHCM(2013), p.239.
#' @param case Case of signalized intersection. Choose one from: \code{'case1'}, \code{'case2'}, \code{'case3'}, \code{'case4'}, \code{'case5'}, \code{'case6'}. See \code{\link{case_si}}
#' @param V_R Right Turn Traffic Volume(vph)
#' @param V_TH Straight-through traffic (vph)
#' @param C Signal cycle(seconds)
#' @param N Total number of access lanes (excluding dedicated left-turn lanes)
#' @keywords traffic going straight right turn public lane signalized intersection
#' @seealso \code{\link{lane_group_si}}
#' @export V_RF_si
#' @examples
#' V_RF_si(case = 'case3', V_R = 432, V_TH = 1293, C = 182, N = 4)
V_RF_si <- function(case = NULL, V_R = NULL, V_TH = NULL, C = NULL, N = NULL){
  if (V_R >= 0 & V_TH >= 0){
    if (C > 0){
      if (N >= 1){
        if (case == 'case1' | case == 'case2' | case == 'case3' | case == 'case4' | case == 'case6'){vrf <- 3600 * V_TH / (C * N * V_R)}
        else if (case == 'case5'){vrf <- 7200 * V_TH / (C * (N - 1) * V_R)
        }
        else {vrf <- 'Error : [case] must be one of [case4], [case5], [case6]. Please check that.'}
      }
      else {vrf <- 'Error : [N] must be positive integer. Please check that.'}
    }
    else {vrf <- 'Error : [C] must be positive(sec). Please check that.'}
  }
  else {vrf <- 'Error : [V_L], [V_TH] must be positive(vph). Please check that.'}
  vrf
}
