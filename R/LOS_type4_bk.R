#' Level of Service for Cyclists at Bicycle Path in Signalized Intersections
#'
#' It calculates Level of Service(LOS) of cyclist at signalised intersections(type4).
#'     * It follows <Table 15-6> in KHCM(2013), p.655
#' @param d Control delay(seconds/veh). See \code{\link{d_bk}}
#' @keywords level of service LOS signalized intersections cyclist bicycle path
#' @export LOS_type4_bk
#' @seealso \code{\link{d_bk}}
#' @examples
#' LOS_type4_bk(32.73)
LOS_type4_bk <- function(d = NULL){
  if (d >= 0 & d < 8){los <- 'A'}
  else if (d >= 8 & d < 12){los <- 'B'}
  else if (d >= 12 & d < 25){los <- 'C'}
  else if (d >= 25 & d < 40){los <- 'D'}
  else if (d >= 40 & d < 55){los <- 'E'}
  else if (d >= 55){los <- 'F'}
  else {los <- 'Error : [d] must be positive(seconds/veh). Please check that.'}
  los
}
