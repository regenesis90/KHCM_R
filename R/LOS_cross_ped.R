#' Pedestrian Level of Service(LOS) at Signal Crosswalk
#'
#' Pedestrian service level at signal crosswalks.
#'     It is determined for the average pedestrian delay that a pedestrian waits to cross the crosswalk,
#'     and the size of the space occupied by the pedestrian crossing the crosswalk.
#'     It follows <Table 14-6> in KHCM(2013), p.619.
#' @param d Average pedestrian delay (sec/person). See \code{\link{d_p_ped}}
#' @export LOS_cross_ped
#' @keywords LOS level of service signalized cross section crossroad crosswalk pedestrian
#' @seealso \code{\link{d_p_ped}}
#' @examples
#' LOS_cross_ped(d = 34.2)
#' LOS_cross_ped(84.56)
LOS_cross_ped <- function(d = NULL){
  if (d >= 0 & d <= 15){los <- 'A'}
  else if (d > 15 & d <= 30){los <- 'B'}
  else if (d > 30 & d <= 45){los <- 'C'}
  else if (d > 45 & d <= 60){los <- 'D'}
  else if (d > 60 & d <= 90){los <- 'E'}
  else if (d > 90){los <- 'F'}
  else {los <- 'Error : [d] must be positive(sec/person). Please check that.'}
  los
}
