#' Peak Hour Traffic Flow Rate at Signalized Intersection
#'
#' Peak hour traffic flow rate (vph) at signal intersections.
#'     It is expressed as 1 hour traffic volume by quadrupling the peak 15-minute traffic volume within the analysis time frame (usually 1 hour peak)
#'     Alternatively, it is obtained by dividing the traffic per hour by the peak hour factor (PHF).
#'     Since the traffic volume to be analyzed is limited to the use of green signals, traffic volumes that turn right on red (RTOR) should be excluded from the analysis.
#'     If peak hour traffic flow rates are measured directly, there is no need to apply PHF.
#'     This function calculates traffic volume converted to peak hour(vph) by using PHF(peak hour factor).
#'     It follows <Formula 8-1> in KHCM(2013), p.223.
#' @param v Hourly traffic volume(vph)
#' @param PHF Peak Hour Factor(PHF).
#' @export V_P_si \code{v/PHF}(vph)
#' @examples
#' V_P_si(1200, 0.98)
#' V_P_si(v = 1300, PHF = 0.93)
V_P_si <- function(v = NULL, PHF = NULL){
  if (v > 0){
    if (PHF >= 0 & PHF < 1){res <- v/PHF}
    else {res <- 'Error : [PHF] must be positive, and less than 1. Please check that. See [PHF_expwy_basic()].'}
  }
  else {res <- 'Error : [v] must be positive(vph). Please check that. See [DDHV()].'}
  res
}
