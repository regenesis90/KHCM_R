#' Average Speed of Passenger Cars in Multi-lane Road, When the Average Traffic Volume is Less than 500 vphpl
#'
#' On a multi-lane road, when the average traffic volume is less than 500 vphpl,
#'     the average speed of passenger cars
#'     It follows <Formula 6-6> in KHCM(2013), p.149.
#' @param v Average traffic volume(vphpl).
#' @param B_SP Maximum travel speed (kph) of a passenger car under basic conditions. See \code{\link{B_SP_ml}}
#' @param F_WC The One Side Lane Width and Side Clearance Speed Correction Factor. See \code{\link{F_wc_ml}}
#' @param F_B Bendiness Speed Correction Factor. See \code{\link{F_B_ml}}
#' @param F_H Speed Correction Factor from Hillness. See \code{\link{F_H_ml}}
#' @param F_A Speed Correction Factor from Number of inflow & outflow Points. See \code{\link{F_A_ml}}
#' @param F_S Speed Correction Factor from Traffic Light. See \code{\link{F_S_ml}}
#' @param F_V Speed correction factor from traffic volume. See \code{\link{F_V_ml}}
#' @export S_P2_ml Average Speed of Passenger Cars in Multi-lane Road, When the Average Traffic Volume is Less than 500 vphpl(kph).
#' @seealso \code{\link{B_SP_ml}}, \code{\link{F_B_ml}}, \code{\link{F_H_ml}}, \code{\link{F_A_ml}}, \code{\link{F_wc_ml}}, \code{\link{F_S_ml}}
#' @examples
#' S_P2_ml(489, 87, 4, 2, 8, 3, 2, 1)
S_P2_ml <- function(v = NULL, B_SP = NULL, F_WC = NULL, F_B = NULL, F_H = NULL, F_A = NULL, F_S = NULL, F_V = NULL){
  S_P1 <- S_P1_ml(B_SP = B_SP, F_WC = F_WC, F_B = F_B, F_H = F_H, F_A = F_A)
  if (v > 500){
    if (is.numeric(S_P1) == TRUE){
      if (F_S >= 0){res <- S_P1 - F_S - F_V}
      else {res <- 'Error : [F_S] must be positive. See [F_S_ml()]. Please check that.'}
    }
    else {res <- S_P1}
  }
  else if (v <= 500 & v >= 0){
    if (is.numeric(S_P1) == TRUE){
      if (F_S >= 0){res <- S_P1 - F_S}
      else {res <- 'Error : [F_S] must be positive. See [F_S_ml()]. Please check that.'}
    }
    else {res <- S_P1}
  }
  else {res <- 'Error : [v] must be positive(vphpl). Please check that.'}
  res
}
