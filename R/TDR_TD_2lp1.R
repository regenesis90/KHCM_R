#' Bi-directional Total Delay Rate in a 2+1 Lane Road
#'
#' Bi-directional total delay rate(TDR) in 2+lane road.
#' This function follows <Formula 7-14> in KHCM(2013), p.186.
#' @param TDR_in TDR in the direction of progress(%).
#' @param TDR_out TDR in opposite direction(%).
#' @param L_in Section length for analysis in the direction of progress(m)
#' @param L_out Section length for analysis in the opposite direction(m)
#' @keywords
#' @export TDR_TD_2lp1 Total Delay Rate for i Sections in One Direction on a 2+1 Lane Road(TDR_2lane_plus1, %)
#' @examples
#' TDR_TD_2lp1(10.8, 2.1, 3.4, 3.2)
TDR_TD_2lp1 <- function(TDR_in = NULL, TDR_out = NULL, L_in = NULL, L_out = NULL){
  if (TDR_in >= 0 & TDR_out >= 0 & L_in >= 0 & L_out >= 0){
    result <- ((TDR_in * L_in) + (TDR_out * L_out))/(L_in + L_out)
    result
  }
  else {result <- 'Error : [TDR_in], [TDR_out], [L_in], [L_out] must be positive. Please check that.'}
}
