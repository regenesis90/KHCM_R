#' Bi-directional Average travel speed of a 2+1 lane road in Total Section
#'
#' Bi-directional average travel speed in 2+l lane road(kph)
#'     This function follows <Formula 7-16> in KHCM(2013), p.188.
#' @param ATS_in ATS in the direction of progress(kph).
#' @param ATS_out ATS in opposite direction(kph).
#' @param L_in Section length for analysis in the direction of progress(m)
#' @param L_out Section length for analysis in the opposite direction(m)
#' @keywords
#' @export ATS_TD_2lp1
#' @examples
#' ATS_TD_2lp1(70, 55, 0.4, 1.3)
ATS_TD_2lp1 <- function(ATS_in = NULL, ATS_out = NULL, L_in = NULL, L_out = NULL){
  if (ATS_in >= 0 & ATS_out >= 0 & L_in >= 0 & L_out >= 0){
    result <- (L_in + L_out) / ((L_in / ATS_in) + (L_out / ATS_out))
  }
  else {result <- 'Error : [ATS_in], [ATS_out], [L_in], [L_out] must be positive. Please check that.'}
  result
}
