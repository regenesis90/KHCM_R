#' Interlocking and Corrected Uniform Control Delay on Arterial Roads
#'
#' Interlocking and corrected uniform control delay on arterial roads(sec/veh).
#'     It follows <Formula 12-3> in KHCM(2013), p.538.
#' @param C Signal period(seconds)
#' @param g Effective green time allocated to the relevant lane group (seconds)
#' @param X The saturation of the lane group
#' @seealso \code{\link{d_artl}}, \code{\link{d_2_artl}}, \code{\link{d_3_artl}}, \code{\link{PF_rsp_artl}}, \code{\link{PF_fix_artl}}, \code{\link{f_cw_artl}}
#' @export d_1_artl Uniform delay
#' @examples
#' d_1_artl(C = 120, g = 42, X = 30)
d_1_artl <- function(C = NULL, g = NULL, X = NULL){
  if (C > 0 & g > 0 & C > g){
    if (is.numeric(X)){
      if (X >= 1){d1 <- (0.5 * C * (1 - g/C)**2)/(1 - g/C)}
      else if (X < 1 & X >= 0){d1 <- (0.5 * C * (1 - g/C)**2)/(1 - X * g/C)}
      else {d1 <- 'Error : [X] must be positive. Please check that.'}
    }
    else {d1 <- 'Error : [X] must be positive. Please check that.'}
  }
  else {d1 <- 'Error : [C], [g] must be positive(sec). And [C] must be greater than [g]. Please check that.'}
  d1
}
