#' Total Average Delay at Two-point Diamond-shaped Interchange
#'
#' It is calculated using the weighted average of traffic volume
#'     for the average delay at two-point diamond-shaped interchange.
#'     It follows <Formula 9-12> in KHCM(2013), p.435.
#' @param d_i Series of average control delay per vehicle of the type point movement flow i(sec/veh). See \code{\link{d_i_rab}}
#' @param v_i Series of corrected traffic volume (vph) of the moving flow i of the end point
#' @keywords total average delay diamond interchange
#' @seealso \code{\link{LOS_di}}
#' @export d_total_di
#' @examples
#' d_total_di(d_i = c(11.1, 3.44, 30.289), v_i = c(1200, 394, 888))
d_total_di <- function(d_i = NULL, v_i = NULL){
  dvsum <- 0
  vsum <- 0
  for (i in 1:length(d_i)){
    dv <- d_i[i] * v_i[i]
    v <- v_i[i]
    vsum <- vsum + v
    dvsum <- dvsum + dv
  }
  dvsum / vsum
}
