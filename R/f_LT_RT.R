#' Turn correction factor in integrated lane(f_LT_RT)
#'
#' This function follows <Formula 8-32> in KHCM(2013)
#' @param E_L
#' @param E_R
#' @param P_LT
#' @param P_RT
#' @keywords
#' @export f_LT_RT Right turn correction factor
#' @examples
f_LT_RT <- function(E_L = NULL, E_R = NULL, P_LT = NULL, P_RT = NULL){
  1/(1 + P_LT * (E_L - 1) + P_RT * (E_R - 1))
}
