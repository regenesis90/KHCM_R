#' The Volume-to-Capacity ratio (v_c_ratio, Vp/C)
#'
#' The Volume-to-Capacity ratio (Vp/C) measures the level of congestion on a roadway by dividing the volume (VPD) of traffic (existing or future) by the capacity of the roadway.
#' @param Vp *Numeric* Traffic Volume Converted to Peak Hour(Vp, vph)
#' @param c *Numeric* Capacity
#' @export v_c_ratio \code{Vp/C} The Volume-to-Capacity ratio
#' @examples
#' SFi(c_j = 1400, v_c_ratio_i = 0.2, N = 6, f_w = 0.5, f_hv = 0.8)
SFi <- function(c_j = NULL, v_c_ratio_i = NULL, N = NULL, f_w = NULL, f_hv = NULL){
  if (c_j >=0 & v_c_ratio_i >=0 & N >= 2 & f_w >=0 & f_hv >=0){
    c_j * v_c_ratio_i * N * f_w * f_hv
  }
}
