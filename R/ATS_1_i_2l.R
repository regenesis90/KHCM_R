#' Average Travel Speed in Type1 2-lane Road, i Section
#'
#' Average travel speed (km/h) in section i of section type 1 on a two-lane road.
#'     This function follows <Formula 7-6> in KHCM(2013), p.174.
#' @param FFS Free speed(kph)
#' @param v_d Traffic volumein the direction of travel(vph)
#' @param v_o Opposite direction traffic volume(vph)
#' @param P_TO Proportion of no overtaking section. Choose one from : \code{0.2}, \code{0.4}, \code{0.6}, \code{0.8}, \code{1.0}
#' @param side_clearance The side clearance of lane(m). From the center line of the center line to the pavement edge of the lane. The Average of left side clearance and right lateral clearance.
#' @param lane_width The width of each lane(m). It must be more than 2.75
#' @keywords Average Travel Speed ATS 2-lane Road
#' @export ATS_1_i_2l Average travel speed in type1 2-lane road, i section(kph)
#' @examples
#' ATS_1_i_2l(FFS = 100, v_d = 1000, v_o = 600, P_TO = 0.8, side_clearance = 2.0, lane_width = 3.25)
ATS_1_i_2l <- function(FFS = NULL, v_d = NULL, v_o = NULL, P_TO = NULL, side_clearance = NULL, lane_width = NULL){
  f_w_ATS <- f_w_ATS_2l(side_clearance = side_clearance, lane_width = lane_width)
  f_np_ATS <- f_np_ATS_2l(v_d = v_d, v_o = v_o, P_TO = P_TO)
  if (is.numeric(f_w_ATS) == TRUE){
    if (is.numeric(f_np_ATS) == TRUE){
      ATS <- FFS - 0.0132 * V_d - 0.0037 * V_o - f_np_ATS - f_w_ATS
    }
    else {ATS <- f_np_ATS}
  }
  else {ATS <- f_w_ATS}
  ATS
}
