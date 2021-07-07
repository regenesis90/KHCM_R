#' Peak Hour Traffic Flow Rate in Arterial Road
#'
#' Peak hour traffic flow rate (vph) on arterial roads.
#'    It follows <Formula 12-6> in KHCM(2013), p.540.
#' @param V_H Hourly traffic volume(vph)
#' @param PHF Peak Hour Factor(PHF).
#' @keywords traffic volume passenger car arterial road
#' @export V_P_artl
#' @examples
V_P_artl <- function(V_H = NULL, PHF = NULL){
  if (V_H > 0){
    if (PHF > 0 & PHF <= 1){v <- V_H / PHF}
    else {v <- 'Error : [PHF] must be >= 0 and <= 1. Please check that.'}
  }
  else {v <- 'Error : [V_H] must be positive(vph). Please check that.'}
  v
}
