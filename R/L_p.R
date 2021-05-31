#' Interference time due to parking activities(L_p)
#'
#' This function follows <Formula 8-9> in KHCM(2013)
#' @param street_parking Choose one from: \code{'allowed'}, \code{'not_allowed'}
#' @param V_park Parking activity per hour (vph)
#' @keywords
#' @export L_p Incremental value (seconds) of saturation head time for right turn due to parking activity
#' @examples
#' L_p(street_parking = 'allowed', V_park = 30)
#' L_p('not_allowed')
L_p <- function(street_parking = NULL, V_park){
  if (street_parking == 'allowed'){l <- 360 + 18 * V_park}
  if (street_parking == 'not_allowed'){l <- 0}
  l
}
