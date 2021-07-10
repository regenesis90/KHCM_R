#' Traffic Volume Converted to Peak Hour in Roundabout
#'
#' Peak traffic volume (vph) of the roundabout i ramp.
#'     It is converted to the peak hour traffic flow rate using the peak time factor (PHF).
#'     It follows <Forumla 11-3> in KHCM(2013), p.499.
#' @param v_i i ramp traffic (vph)
#' @param PHF Peak Hour Factor(PHF). It is a value between 0 and 1.
#' @export V_P_i_rab i Traffic volume at the peak of the ramp (vph)
#' @examples
#' V_P_i_rab(1000, 0.97)
#' V_P_i_rab(v_i = 1300, PHF = 0.93)
V_P_i_rab <- function(v_i = NULL, PHF = NULL){
  if (v_i >= 0){
    if (PHF > 0 & PHF <= 1){v <- v_i / PHF}
    else {v <- 'Error : [PHF] must be > 0 and <= 1. Please check that.'}
  }
  else {v <- 'Error : [v_i] must be positive(vph). Please check that.'}
  v
}
