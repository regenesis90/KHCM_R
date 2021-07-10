#' Heavy Vehicle Factors in Roundabout
#'
#' It follows <Formula 11-4> in KHCM(2013) p.500
#' @param lane Roundabout lane. Choose one from : \code{1}, \code{2}
#' @param P_T Heavy Vehicle Ratio.
#' @keywords Heavy Vehicle Fators Roundabout
#' @seealso \code{\link{E_T_rab}}, \code{\link{V_i_pce_rab}}
#' @export f_hv_rab Heavy Vehicle Factors in Roundabout
#' @examples
#' f_hv_rab(lane = 2, P_T = 0.3)
f_hv_rab <- function(lane = NULL, P_T = NULL){
  et <- E_T_rab(lane = lane, hv_pcn = P_T * 100)
  if (is.numeric(et) == TRUE){
    if (P_T >= 0 & P_T <= 1){fhv <- 1 / (1 + P_T * (et - 1))}
    else {fhv <- 'Error : [P_T] must be >= 0 and <= 1. Please check that.'}
  }
  else {fhv <- et}
  fhv
}
