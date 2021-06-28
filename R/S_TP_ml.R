#' Average Speed of Total Vehicles in Multi-lane Road, When the Average Traffic Volume is Less than 500 vphpl
#'
#' On a multi-lane road, the average speed of Total vehicles.
#'     It follows <Formula 6-8> in KHCM(2013), p.149.
#' @param S_P2 Average speed of passenger cars in multi-lane road, when the average traffic volume(kph).
#' @param S_T2 Average speed of heavy vehicles in multi-lane road, when the average traffic volume(kph).
#' @param p Ratio of Heavy vehicle
#' @export S_TP_ml Average speed of total vehicles in multi-lane road, when the average traffic volume is less than 500 vphpl (kph).
#' @seealso \code{\link{S_P2_ml}}, \code{\link{S_T2_ml}}
#' @examples
#' S_TP_ml(33.48, 29.19, 0.3)
S_TP_ml <- function(S_P2 = NULL, S_T2 = NULL, p = NULL){
  if (S_P2 >= 0 & S_T2 >= 0){
    if (p >= 0 & p <= 1){res <- (1 - p) * S_P2 + p * S_T2}
    else {res <- 'Error : [p] must be >= 0 and <= 1. Please check that.'}
  }
  else {res <- 'Error : [S_P2], [S_T2] must be positive. Please check that. See [S_P2_ml()], [S_T2_ml()].'}
  res
}
