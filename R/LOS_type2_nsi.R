#' Level of Service of Unsignalized Two-way Stop Intersection
#'
#' Level of service(LOS) of two-way stop intersection (type 2) among non-signaled intersections.
#'     This function follows <Table 10-2> in KHCM(2013) p.470.
#' @param d Average operating delay (sec/veh). See \code{\link{d_x_nsi}}
#' @keywords Level of service LOS unsignalized non-signalized two-way stop intersection
#' @seealso \code{\link{d_x_nsi}}
#' @details
#'     Considering the traffic flow characteristics of non-signaled intersections,
#'     the service level E state cannot be defined as a capacity state.
#' @export LOS_type2_nsi
#' @examples
#' LOS_type2_nsi(d = 33.2)
#' LOS_type2_nsi(18.45)
LOS_type2_nsi <- function(d = NULL){
  if (d > 0 & d <= 10){los <- 'A'}
  else if (d > 10 & d <= 15){los <- 'B'}
  else if (d > 15 & d <= 25){los <- 'C'}
  else if (d > 25 & d <= 35){los <- 'D'}
  else if (d > 35 & d <= 50){los <- 'E'}
  else if (d > 50){los <- 'F'}
  else {los <- 'Error : [d] must be positive(sec/veh). Please check that.'}
  los
}
