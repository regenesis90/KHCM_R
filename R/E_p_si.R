#' Straight Conversion Factor by Left Turn Curve Radius at Signalized Intersection
#'
#' Straight conversion factor by left turn curve radius at signalized intersection.
#'     When turning left, the saturation traffic flow rate changes according to the radius of the curve.
#'     This function follows <Table 8-9> in KHCM(2013), p.228.
#' @param radius Left turn curve radius(m)
#' @keywords radius straight conversion factor left turn curve signalized intersection
#' @seealso \code{\link{E_L_si}}
#' @export E_p_si Straight conversion factor by left turn curve radius
#' @examples
#' E_p_si(radius = 8.22)
E_p_si <- function(radius = NULL){
  if (radius >= 0 & radius <= 9){e <- 1.14}
  else if (radius > 9 & radius <= 12){e <- 1.11}
  else if (radius > 12 & radius <= 15){e <- 1.09}
  else if (radius > 15 & radius <= 18){e <- 1.06}
  else if (radius > 18 & radius <= 20){e <- 1.05}
  else if (radius > 20){e <- 1.00}
  else {e <- 'Error : [radius] must be positive(m). Please check that.'}
  e
}
