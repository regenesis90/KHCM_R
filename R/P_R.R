#' The ratio of left turns in the group of practically dedicated right-turn lanes(P_R)
#'
#' This function follows <Formula 8-22> in KHCM(2013)
#' @param V_R
#' @param V_RF
#' @keywords
#' @export P_R The ratio of left turns in the group of practically dedicated right-turn lanes
#' @examples
#' P_R(V_R = 321, V_RF = 1289)
P_R <- function(V_R = NULL, V_RF = NULL){
  if (V_R >= 0 & V_RF >= 0 & ((V_R + V_RF) > 0)){
    p <- V_R / (V_RF + V_R)
    p
  }
}
