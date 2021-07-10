#' Level of Service in Uncontrolled Unsignalized intersection
#'
#' Level of service(LOS) of uncontrolled intersections(type 1) among unsignalized intersections.
#'     This function follows <Table 10-4> in KHCM(2013) p.473.
#' @param r Traffic volume ratio of main road(%)
#' @param v Total intersection traffic volume(vph)
#' @keywords Level of Service LOS uncontrolled unsignalized intersection
#' @details
#'     * Considering the traffic flow characteristics of unsignalized intersections, the service level E state cannot be defined as a capacity state.
#'     * The total traffic volume at the intersection is the sum of the traffic entering the intersection in all directions.
#' @seealso \code{\link{conflict_nsi}}
#' @export LOS_type1_nsi
#' @examples
#' LOS_type1_nsi(r = 48, v = 394)
LOS_type1_nsi <- function(r = NULL, v = NULL){
  if (r >= 0 & r < 60){
    if (v >= 0 & v <= 320){los <- 'A'}
    else if (v > 320 & v <= 640){los <- 'B'}
    else if (v > 640 & v <= 960){los <- 'C'}
    else if (v > 960 & v <= 1280){los <- 'D'}
    else if (v > 1280 & v <= 1600){los <- 'E'}
    else if (v > 1600){los <- 'F'}
    else {los <- 'Error : [v] must be positive(vph). Please check that.'}
  }
  else if (r >= 60 & r < 70){
    if (v >= 0 & v <= 360){los <- 'A'}
    else if (v > 360 & v <= 720){los <- 'B'}
    else if (v > 720 & v <= 1080){los <- 'C'}
    else if (v > 1080 & v <= 1440){los <- 'D'}
    else if (v > 1440 & v <= 1800){los <- 'E'}
    else if (v > 1800){los <- 'F'}
    else {los <- 'Error : [v] must be positive(vph). Please check that.'}
  }
  else if (r >= 70 & r <= 100){
    if (v >= 0 & v <= 400){los <- 'A'}
    else if (v > 400 & v <= 800){los <- 'B'}
    else if (v > 800 & v <= 1200){los <- 'C'}
    else if (v > 1200 & v <= 1600){los <- 'D'}
    else if (v > 1600 & v <= 2000){los <- 'E'}
    else if (v > 2000){los <- 'F'}
    else {los <- 'Error : [v] must be positive(vph). Please check that.'}
  }
  else {los <- 'Error : [r] must be >= 0 and <= 100(%). Please check that.'}
  los
}
