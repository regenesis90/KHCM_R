#' Average Speed of Heavy Vehicles in Multi-lane Road, When the Average Traffic Volume is Less than 500 vphpl
#'
#' On a multi-lane road, the average speed of heavy vehicles.
#'     It follows <Formula 6-7>, <Formula 6-10> in KHCM(2013), p.149-150.
#' @param v Average traffic volume(vphpl).
#' @param F_WC The One Side Lane Width and Side Clearance Speed Correction Factor. See \code{\link{F_wc_ml}}
#' @param F_H Speed correction factor from hillness. See \code{\link{F_H_ml}}
#' @param F_A Speed correction factor from number of inflow & outflow Points. See \code{\link{F_A_ml}}
#' @param F_S Speed correction factor from traffic light. See \code{\link{F_S_ml}}
#' @param F_V Speed correction factor from traffic volume. See \code{\link{F_V_ml}}
#' @export S_T2_ml Average Speed of Heavy Vehicles in Multi-lane Road, When the Average Traffic Volume is Less than 500 vphpl (kph).
#' @seealso \code{\link{F_B_ml}}, \code{\link{F_A_ml}}, \code{\link{F_wc_ml}}, \code{\link{F_S_ml}}
#' @examples
#' S_T2_ml(499, 4, 4, 3, 5, 2)
#' S_T2_ml(501, 4, 4, 3, 5, 2)
S_T2_ml <- function(v = NULL, F_WC = NULL, F_H = NULL, F_A = NULL, F_S = NULL, F_V = NULL){
  if (v > 500){
    if (F_WC >= 0 & F_H >= 0 & F_A >= 0 & F_S >= 0 & F_V >= 0){
      res <- 80 - F_WC - F_H - F_A - F_S - F_V
    }
    else {res <- 'Error : [F_WC], [F_H], [F_A], [F_S] must be positive. See [F_wc_ml()], [F_H_ml()], [F_A_ml()], [F_S_ml()], [F_V_ml()]. Please check that.'}
  }
  else if (v <= 500 & v >= 0){
    if (F_WC >= 0 & F_H >= 0 & F_A >= 0 & F_S >= 0){
      res <- 80 - F_WC - F_H - F_A - F_S
    }
    else {res <- 'Error : [F_WC], [F_H], [F_A], [F_S] must be positive. See [F_wc_ml()], [F_H_ml()], [F_A_ml()], [F_S_ml()]. Please check that.'}
  }
  else {res <- 'Error : [v] must be positive(vphpl). Please check that.'}
  res
}
