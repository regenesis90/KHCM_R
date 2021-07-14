#' Interference Time Due to Parking Activity at Signalized Intersection
#'
#' Interference time due to parking activity at the signalized intersection.
#'     Incremental value (seconds) of saturation head time for right turn due to parking activity.
#'     If it is possible to park near the signal intersection,
#'     the normal traffic of vehicles will be disturbed by the parking activity,
#'     and the saturation traffic flow rate will decrease.
#'     This effect only occurs within 75 m of the stop line.
#'     This function follows <Formula 8-9> in KHCM(2013), p.231.
#' @param street_parking Whether on-street parking is allowed. Choose one from: \code{'yes'}, \code{'no'}
#' @param v_park Parking activity per hour (vph)
#' @keywords Interference time parking signalized intersection
#' @seealso \code{\link{L_H_si}}
#' @export L_p Incremental value (seconds) of saturation head time for right turn due to parking activity
#' @examples
#' L_p_si(street_parking = 'yes', v_park = 30)
#' L_p_si(street_parking = 'no)
L_p_si <- function(street_parking = NULL, v_park){
  if (street_parking == 'yes'){
    if (v_park >= 0){l <- 360 + 18 * v_park}
    else {l <- 'Error : [v_park] must be >= 0(vph). Please check that.'}
  }
  else if (street_parking == 'no'){l <- 0}
  else {l <- 'Error : [street_parking] must be one of [yes], [no]. Please check that.'}
  l
}
