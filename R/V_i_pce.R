#' Traffic Volume Entering the i-direction at Roundabout
#'
#' Traffic volume entering the i-direction at roundabouts (pcph).
#'     It follows <Formula 11-4> in KHCM(2013) p.500.
#' @param v_p_i i ramp traffic volume (vph)
#' @param lane Roundabout lane. Choose one from : \code{1}, \code{2}
#' @param P_T Heavy Vehicle Ratio.
#' @param P_T Heavy Vehicle Mixing Ratio
#' @export V_i_pce
#' @keywords traffic volume direction roundabout
#' @seealso \code{\link{f_hv_rab}}, \code{\link{E_T_rab}}
#' @examples
#' V_i_pce(v_p_i = 1235, lane = 1, P_T = 0.3)
V_i_pce <- function(v_p_i = NULL, lane = NULL, P_T = NULL){
  fhv <- f_hv_rab(lane = lane, P_T = lane)
  if (is.numeric(fhv) == TRUE){
    if (v_p_i >= 0){v <- v_p_i / fhv}
    else {v <- 'Error : [v_p_i] must be positive(vph). Please check that.'}
  }
  else {v <- fhv}
  v
}
