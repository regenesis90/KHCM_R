#' Heavy Vehicle Correction Factor at Signalized Intersection
#'
#' Heavy vehicle correction factor at signalized intersection.
#'     In order to directly use the surveyed traffic volume as the actual traffic volume,
#'     the saturation traffic flow rate is corrected.
#'     The average passenger car conversion factor of 1.8 is used
#'     considering the mixing rate of all heavy vehicles other than passenger cars.
#'     It follows <Formula 8-39> in KHCM(2013), p.249.
#' @param p Mixing ratio of heavy vehicles to actual traffic volume
#' @export f_hv_si Heavy vehicle Factor
#' @examples
#' f_hv_si(p = 0.23)
f_hv_si <- function(p = NULL){
  if (p >= 0 & p <= 1){f <- 1 / (1 + 0.8 * p)}
  else {f <- 'Error : [p] must be >= 0 and <= 1. Please check that.'}
  f
}
