#' Average Maximum Travel Speed that a Passenger Car can Achieve in Multi-lane Road
#'
#' Average maximum travel speed that a passenger car can achieve (kph).
#'     The average maximum travel speed that a passenger car can achieve
#'     at a level of 500 vphpl or less, given road conditions.
#'     Road conditions include lane width, lateral clearance, planar and vertical alignment,
#'     and the number of points of entry and exit.
#'     It follows <Formula 6-5> in KHCM(2013), p.148.
#' @param B_SP Maximum travel speed (kph) of a passenger car under basic conditions. See \code{\link{B_SP_ml}}
#' @param F_WC The One Side Lane Width and Side Clearance Speed Correction Factor. See \code{\link{F_wc_ml}}
#' @param F_B Bendiness Speed Correction Factor. See \code{\link{F_B_ml}}
#' @param F_H Speed Correction Factor from Hillness. See \code{\link{F_H_ml}}
#' @param F_A Speed Correction Factor from Number of inflow & outflow Points. See \code{\link{F_A_ml}}
#' @export S_P1_ml The maximum travel speed of a passenger car for a given road condition (kph).
#' @seealso \code{\link{B_SP_ml}}, \code{\link{F_B_ml}}, \code{\link{F_H_ml}}, \code{\link{F_A_ml}}, \code{\link{F_wc_ml}}
#' @examples
#' S_P1_ml(87, 4, 2, 8, 3)
S_P1_ml <- function(B_SP = NULL, F_WC = NULL, F_B = NULL, F_H = NULL, F_A = NULL){
  if (B_SP >= 0 & F_WC >= 0 & F_B >= 0 & F_H >= 0 & F_A >= 0){
    res <- B_SP - F_WC - F_B - F_H - F_A
  }
  else {res <- 'Error : [B_SP], [F_WC], [F_B], [F_H], [F_A] must be positive. Please check that.'}
  res
}
