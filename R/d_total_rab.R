#' Total Average Delay at Roundabouts
#'
#' It is calculated using the weighted average of traffic volume
#'     for the average delay for each approach i of the roundabout.
#'     It follows <Formula 11-10>, <Formula 11-12> in KHCM(2013), p.503, 506.
#' @param d_i Series of average control delay per vehicle of the type point movement flow i(sec/veh). See \code{\link{d_i_rab}}
#' @param v_i Series of corrected traffic volume (vph) of the moving flow i of the end point
#' @keywords total average delay roundabout
#' @seealso \code{\link{d_i_rab}}, \code{\link{LOS_rab}}
#' @export d_total_rab
#' @examples
#' d_total_rab(d_i = c(11.1, 3.44, 30.289), v_i = c(1200, 394, 888))
d_total_rab <- function(d_i = NULL, v_i = NULL){
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
