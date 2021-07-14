#' Loss of Saturation Headway Time Due to Roadside Friction on Right-turn Lanes at Signal Intersections
#'
#' Loss of saturation headway time due to roadside friction on right-turn lanes at signal intersections.
#'     Loss of headway time due to roadside friction on the side lane (seconds).
#'     This function follows <Formula 8-10> in KHCM(2013), p.232.
#' @param L_dw Interference time by vehicles entering and exiting(s). See \code{\link{L_dw_si}}
#' @param L_bb Interference time due to bus stop(s). See \code{\link{L_bb_si}}
#' @param L_p Interference time due to parking activities(s). See \code{\link{L_p_si}}
#' @keywords
#' @details
#'     In the case of one-way traffic, friction on the left side of the road should also be considered.
#'     The roadside friction on the left is calculated in the same way.
#' @export L_H Loss of headway time due to roadside friction in the right lane (seconds)
#' @seealso \code{\link{L_dw_si}}, \code{\link{L_bb_si}}, \code{\link{L_p_si}}
#' @examples
#' L_H(L_dw = 30.2, L_bb = 38.22, L_p = 101.33)
L_H <- function(L_dw = NULL, L_bb = NULL, L_p = NULL){
  if (L_dw >= 0 & L_bb >= 0 & L_p >= 0){
    res <- (L_dw + L_bb + L_p) * 0.3
  }
  else {res <- 'Error : [L_dw], [L_bb], [L_b] must be positive(sec). Please check that.'}
  res
}

