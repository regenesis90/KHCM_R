#' Signal Intersection Approach Slope Correction Factor
#'
#' Signal intersection approach slope correction factor.
#'     The effect of the slope of the approach to the signal intersection on the saturation traffic flow rate.
#'     It follows <Table 8-16> in KHCM(2013)
#' @param gradient Slope gradient(%)
#' @seealso \code{\link{S_i_si}}
#' @details
#'     When measuring the slope of an intersection, the average value of the slopes surveyed near the approach is used.
#'     If the direction of the approach stop line is an upward slope,
#'     the saturation traffic flow rate decreases, and in the case of a downward slope,
#'     it does not change with the flat ground.
#' @export f_g_si
#' @examples
#' f_g_si(gradient = 2.72)
#' f_g_si(3.945)
f_g_si <- function(gradient = NULL){
  if (gradient <= 0){f <- 1.00}
  else if (gradient > 0 & gradient < 3){f <- 1.00 + ((0.96 - 1.00)/3) * (gradient - 0)}
  else if (gradient == 3){f <- 0.96}
  else if (gradient > 3 & gradient < 6){f <- 0.96 + ((0.93 - 0.96)/3) * (gradient - 3)}
  else if (gradient >= 6 & gradient <= 100){f <- 0.93}
  else {f <- 'Error : [gradient] must be positive(%). Please check that.'}
  f
}
