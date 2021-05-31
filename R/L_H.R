#' Comprehensive roadside friction(L_H)
#'
#' This function follows <Formula 8-10> in KHCM(2013)
#' @param L_dw Interference time by vehicles entering and exiting.(s) See L_dw()
#' @param L_bb Interference time due to bus stop(s). See L_bb()
#' @param L_p Interference time due to parking activities(s). See L_p()
#' @keywords
#' @export L_H Loss of headway time due to roadside friction in the right lane (seconds)
#' @examples
#' L_H(30.2, 38.22, 101.33)
L_H <- function(L_dw = NULL, L_bb = NULL, L_p = NULL){
  if (L_dw >= 0 & L_bb >= 0 & L_p >= 0){
    (L_dw + L_bb + L_p) * 0.3
  }
}

