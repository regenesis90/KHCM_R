#' Additional green loss time downstream due to non-use of green time(L_DS)
#'
#' TIt follows <Formula 9-6> in KHCM(2013), p.429
#' @param CG_DS Period in which the upstream intersection shows a left turn and the downstream intersection shows green
#' @param Q_V Average number of waiting vehicles in the internal link at the start of each signal (units)
#' @param h_s Saturation difference time (sec/set)
#' @keywords
#' @export mu_F
#' @examples
L_DS <- function(CG_DS = NULL, Q_V = NULL, h_s = NULL){
  CG_DS - Q_V * h_s
}
