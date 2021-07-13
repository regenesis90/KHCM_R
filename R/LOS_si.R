#' Level of Service(LOS) in Signalized Intersections.
#'
#' Service level at signal intersections.
#'     It is expressed as an average control delay (s) value per vehicle.
#'     It follows <Table 8-2> in KHCM(2013), p.218.
#' @param d Delay due to the reduction or stop of the lane group due to signal control, The increase in travel time compared to travel time when there is no deceleration or stop(seconds).
#' @keywords LOS Level of Service signalized intersection
#' @export LOS_si Level of Service. \code{A}, \code{B}, \code{C}, \code{D}, \code{E}, \code{F}, \code{FF}, \code{FFF}
#' @examples
#' LOS_si(d = 44)
#' LOS_si(157)
LOS_si <- function(d = NULL){
  if (d >= 0 & d <= 15){LOS <- 'A'}
  else if (d > 15 & d <= 30){LOS <- 'B'}
  else if (d > 30 & d <= 50){LOS <- 'C'}
  else if (d > 50 & d <= 70){LOS <- 'D'}
  else if (d > 70 & d <= 100){LOS <- 'E'}
  else if (d > 100 & d <= 220){LOS <- 'F'}
  else if (d > 220 & d <= 340){LOS <- 'FF'}
  else if (d > 340){LOS <- 'FFF'}
  else {LOS <- 'Error : [d] must be positive(seconds). Please check that.'}
  LOS
}
