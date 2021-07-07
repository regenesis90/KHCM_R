#' Average Control Delay Per Vehicle on Arterial Road
#'
#' Average control delay per vehicle on arterial roads (sec/veh).
#'     It follows <Formula 12-2> in KHCM(2013) p.538.
#' @param d_1 Interlocking corrected uniform control delay(sec/veh). See \code{\link{d_1_artl}}
#' @param d_2 Incremental lag indicative of random arrivals and supersaturation. See \code{\link{d_2_artl}}
#' @param d_3 Additional delay (sec/veh). See \code{\link{d_3_artl}}
#' @param PF Interlocking coefficient. See \code{\link{PF_rsp_artl}}, \code{\link{PF_fix_artl}}
#' @param f_cw Pedestrian crossing signal correction factor on arterial road. See \code{\link{f_cw_artl}}
#' @seealso \code{\link{d_1_artl}}, \code{\link{d_2_artl}}, \code{\link{d_3_artl}}, \code{\link{PF_rsp_artl}}, \code{\link{PF_fix_artl}}, \code{\link{f_cw_artl}}
#' @export d_artl Average control delay per vehicle
#' @examples
#' d_artl(d_1 = 32.1, d_2 = 11.1, d_3 = 9.20, PF = 1.43, f_cw = 1.1)
d_artl <- function(d_1 = NULL, d_2 = NULL, d_3 = NULL, PF = NULL, f_cw = 1){
  if (d_1 > 0 & d_2 > 0 & d_3 > 0){
    if (PF > 0){
      if (f_cw == 1.0 | f_cw == 1.1 | f_cw == 1.2){d <- d_1 * PF * f_cw + d_2 + d_3}
      else {d <- 'Error : [f_cw] must be one of 1.0, 1.1, 1.2. Please check that.'}
    }
    else {d <- 'Error : [PF] must be positive. Please check that.'}
  }
  else {d <- 'Error : [d_1], [d_2], [d_3] must be positive. Please check that.'}
  d
}
