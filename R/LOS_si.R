#' Level of Service(LOS) in Signalized Intersections.
#'
#' This function decides Level of Service(LOS). It follows <Table 8-2>
#' @param control_delay *Numeric* (s) Delay due to the reduction or stop of the lane group due to signal control, The increase in travel time compared to travel time when there is no deceleration or stop.
#' @keywords LOS Level of Service
#' @export LOS_signalized_intersection Level of Service. \code{A}, \code{B}, \code{C}, \code{D}, \code{E}, \code{F}, \code{FF}, \code{FFF}
#' @examples
#' LOS_signalized_intersection(control_delay = 44)
#' LOS_signalized_intersection(157)
LOS_signalized_intersection <- function(control_delay = NULL){
  if (control_delay >= 0 & control_delay <= 15){LOS <- 'A'}
  if (control_delay > 15 & control_delay <= 30){LOS <- 'B'}
  if (control_delay > 30 & control_delay <= 50){LOS <- 'C'}
  if (control_delay > 50 & control_delay <= 70){LOS <- 'D'}
  if (control_delay > 70 & control_delay <= 100){LOS <- 'E'}
  if (control_delay > 100 & control_delay <= 220){LOS <- 'F'}
  if (control_delay > 220 & control_delay <= 340){LOS <- 'FF'}
  if (control_delay > 340){LOS <- 'FFF'}
  LOS
}
