#' Heavy Vehicle Factors in Roundabout(f_hv_roundabout)
#'
#' It follows <Formula 11-4> in KHCM(2013) p.500
#' @param lane Choose one from L \code{1}, \code{2}
#' @param hv_ratio Heavy Vehicle Ratio(%)
#' @param P_T Heavy Vehicle Mixing Ratio
#' @export f_hv_roundabout Heavy Vehicle Factors in Roundabout
#' @examples
f_hv_roundabout <- function(lane = NULL, hv_ratio = NULL, P_T = NULL){
  et <- E_T_roundabout(lane = lane, hv_ratio = hv_ratio)
  fhv <- 1 / (1 + P_T * (et - 1))
  fhv
}
