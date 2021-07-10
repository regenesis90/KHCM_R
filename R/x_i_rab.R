#' Saturation of i-Ramp at Roundabout
#'
#' Saturation of i-Ramp at roundabout.
#'     It follows <Formula 11-8> in KHCM(2013), p.502.
#' @param v_i i-ramp traffic volume(vph)
#' @param c_i i-ramp capacity(vph)
#' @keywords sauration roundabout
#' @export x_i_rab Saturation of i-ramp at roundabout
#' @examples
#' x_i_rab(v_i = 344, c_i = 1000)
x_i_rab <- function(v_i = NULL, c_i = NULL){
  if (v_i >= 0 & c_i > 0){x <- v_i/c_i}
  else {x <- 'Error : [v_i], [c_i] must be positive(vph). Please check that.'}
  x
}
