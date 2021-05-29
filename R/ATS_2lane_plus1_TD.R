#' Average travel speed of a 2+1 lane road in Total Section(ATS_2lane_plus1_TD, kph)
#'
#' This function follows <Formula 7-16>
#' @param ATS_2lane_plus1_IN *Numeric* ATS in the direction of progress
#' @param ATS_2lane_plus1_OUT *Numeric* ATS in opposite direction
#' @param L_IN *Numeric* Section length for analysis in the direction of progress(m)
#' @param L_OUT *Numeric* Section length for analysis in the opposite direction(m)
#' @keywords
#' @export ATS_2lane_plus1_TD
#' @examples
#' ATS_2lane_plus1_TD(70, 55, 0.4, 1.3)
ATS_2lane_plus1_TD <- function(ATS_2lane_plus1_IN = NULL, ATS_2lane_plus1_OUT = NULL, L_IN = NULL, L_OUT = NULL){
  if (ATS_2lane_plus1_IN >= 0 & ATS_2lane_plus1_OUT >= 0 & L_IN >= 0 & L_OUT >= 0){
    result <- (L_IN + L_OUT) / ((L_IN / ATS_2lane_plus1_IN) + (L_OUT / ATS_2lane_plus1_OUT))
    result
  }
}
