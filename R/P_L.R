#' The ratio of left turns in the group of practically dedicated left-turn lanes(P_L)
#'
#' This function follows <Formula 8-21> in KHCM(2013)
#' @param V_L
#' @param V_LF
#' @keywords
#' @export P_L The ratio of left turns in the group of practically dedicated left-turn lanes
#' @examples
#' P_L(320, 832)
P_L <- function(V_L = NULL, V_LF = NULL){
  if (V_L >= 0 & V_LF >= 0 & ((V_L + V_LF) > 0)){
    p <- V_L / (V_LF + V_L)
    p
  }
}
