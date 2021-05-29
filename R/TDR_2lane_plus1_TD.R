#' Total Delay Rate Entire section in a 2+1 Lane Road(TDR_2lane_plus1_TD, %)
#'
#' This function follows <Formula 7-14>
#' @param TDR_2lane_plus1_IN *Numeric* TDR in the direction of progress
#' @param TDR_2lane_plus1_OUT *Numeric* TDR in opposite direction
#' @param L_IN *Numeric* Section length for analysis in the direction of progress(m)
#' @param L_OUT *Numeric* Section length for analysis in the opposite direction(m)
#' @keywords
#' @export TDR_2lane_plus1_TD Total Delay Rate for i Sections in One Direction on a 2+1 Lane Road(TDR_2lane_plus1, %)
#' @examples
#' TDR_2lane_plus1_TD(10.8, 2.1, 3.4, 3.2)
TDR_2lane_plus1_TD <- function(TDR_2lane_plus1_IN = NULL, TDR_2lane_plus1_OUT = NULL, L_IN = NULL, L_OUT = NULL){
  if (TDR_2lane_plus1_IN >= 0 & TDR_2lane_plus1_OUT >= 0 & L_IN >= 0 & L_OUT >= 0){
    result <- ((TDR_2lane_plus1_IN * L_IN) + (TDR_2lane_plus1_OUT * L_OUT))/(L_IN + L_OUT)
    result
  }
}
