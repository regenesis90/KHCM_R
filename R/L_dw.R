#' Interference time by vehicles entering and exiting(L_dw)
#'
#' This function follows <Formula 8-7> in KHCM(2013)
#' @param V_en Traffic entering the arterial road (vph)
#' @param V_ex Traffic leaving the arterial road (vph)
#' @keywords
#' @export L_dw Loss time per hour due to in and out of the back road (seconds)
#' @examples
#' L_dw(V_en = 800, V_ex = 732)
#' L_dw(132, 241)
L_dw <- function(V_en = NULL, V_ex = NULL){
  0.9 * V_en + 1.4 * V_ex
}
