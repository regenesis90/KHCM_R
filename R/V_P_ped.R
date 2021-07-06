#' Peak-Converted Pedestrian Traffic Flow Rate
#'
#' Peak-converted pedestrian traffic flow rate.(person/min/m)
#'     The peak 15-minute pedestrian walking traffic volume surveyed was converted to the pedestrian traffic flow rate (person/min/m).
#'     This value is used to determine the LOS using \code{\link{LOS_road_ped}}.
#'     It follows <Formula 14-4> in KHCM(2013), p.620.
#' @param V_15 Pedestrian traffic volume during peak 15 minutes(person/15min/m)
#' @param W_E Effective pedestrian sidewalk width(m). See \code{\link{W_E_ped}}
#' @keywords peak hour converted pedestrian traffic flow rate
#' @seealso \code{\link{LOS_road_ped}}, \code{\link{W_E_ped}}
#' @export V_P_ped pedestrian traffic flow rate (person/min/m)
#' @examples
#' V_P_ped(V_15 = 393, W_E = 3.2)
V_P_ped <- function(V_15 = NULL, W_E = NULL){
  if (V_15 >= 0){
    if (W_E > 0){
      vp <- V_15 / (15 * W_E)
    }
    else {vp <- 'Error : [W_E] must be positive(m). Please check that.'}
  }
  else {vp <- 'Error : [V_15] must be positive(person/15min/m). Please check that.'}
  vp
}
