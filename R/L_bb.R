#' Interference time due to bus stop(L_bb)
#'
#' This function follows <Formula 8-8> in KHCM(2013)
#' @param T_b Saturation head time increment according to one bus stop (sec). See T_b()
#' @param l Distance from stop line to bus stop (m)
#' @param V_b Number of bus stops per hour
#' @keywords
#' @export L_bb Time lost per hour due to bus stop (seconds)
#' @examples
#' L_bb(T_b = 10.8, l = 30, V_b = 12)
#' L_bb(1.4, 15, 15)
L_bb <- function(T_b = NULL, l = NULL, V_b = NULL){
  if (T_b >= 0 & l >= 0 & V_b >= 0){
    if (l < 75){lbb <- T_b * (75 - l)/75 * V_b}
    else{lbb <- 0}
    lbb
  }
}
