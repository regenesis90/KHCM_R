#' Left Turn Traffic Volume Arriving Before the First Left Turn in Shared Lane for 3-way Signalized Intersection
#'
#' If there is a shared lane for turning left on an approach with only straight and left turns at a 3-way signalized intersection,
#'     the amount of straight-forward traffic arriving before the first left turn (vph).
#'     This function follows <Formula 8-14> in KHCM(2013), p.245.
#' @param case Case of signalized intersection. Choose one from: \code{'case4'}, \code{'case5'}, \code{'case6'}. See \code{\link{case_si}}
#' @param V_L Left Turn Traffic Volume(vph)
#' @param V_TH Straight-through traffic (vph)
#' @param C Signal cycle(seconds)
#' @param N Total number of access lanes (excluding dedicated left-turn lanes).
#' @keywords traffic going straight left turn public lane 3-way signalized intersection
#' @seealso \code{\link{P_L_pd_3si}}
#' @export V_LF_si
#' @examples
#' V_LF_pb_3si(case = 'case5', V_L = 304, V_TH = 999, C = 194, N = 3)
V_LF_pb_3si <- function(case = NULL, V_L = NULL, V_TH = NULL, C = NULL, N = NULL){
  if (V_L >= 0 & V_TH >= 0){
    if (C > 0){
      if (N >= 1){
        if (case == 'case4' | case == 'case6'){vlf <- 3600 * V_TH / (C * N * V_L)}
        else if (case == 'case5'){vlf <- 7200 * V_TH / (C * (N - 1) * V_L)
        }
        else {vlf <- 'Error : [case] must be one of [case4], [case5], [case6]. Please check that.'}
      }
      else {vlf <- 'Error : [N] must be positive integer. Please check that.'}
    }
    else {vlf <- 'Error : [C] must be positive(sec). Please check that.'}
  }
  else {vlf <- 'Error : [V_L], [V_TH] must be positive(vph). Please check that.'}
  vlf
}
