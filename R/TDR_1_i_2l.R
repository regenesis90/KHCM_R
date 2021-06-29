#' Total Delay Rate in Section i of Section Type1 in 2-lane Road
#'
#' Total delay rate (%) in section i of section type 1 of the two-lane road
#'    It follows <Formula 7-5> in KHCM(2013), p.174.
#' @param v_d Traffic Volume in direction(pc/lane).
#' @param v_o Opposite traffic volume(vph). Choose one from : \code{200}, \code{400}, \code{600}, \code{800}, \code{1000}, \code{1200}, \code{1400}, \code{1600>=}
#' @param P_TO Proportion of no overtaking section. Choose one from : \code{0.2}, \code{0.4}, \code{0.6}, \code{0.8}, \code{1.0}
#' @param side_clearance The side clearance of lane(m). From the center line of the center line to the pavement edge of the lane. The Average of left side clearance and right lateral clearance.
#' @param lane_width The width of each lane(m). It must be more than 2.75
#' @keywords TDR Total Delay Rate
#' @export TDR_1_i_2l Total delay rate (%) in section i of section type 1 of the two-lane road
#' @details The total delay rate is the average delay rate of vehicles
#'     within a vehicle group traveling in a certain section.
#'     The total delay rate is a measure that expresses the degree of delay with respect
#'     to the speed desired by the driver.
#'     When traffic is low, vehicles are rarely delayed,
#'     and the average head distance is also increased,
#'     which increases the likelihood of overtaking.
#'     Although the total delay rate is low under low traffic conditions,
#'     the chance of overtaking decreases as the vehicle approaches capacity,
#'     so that almost all vehicles form a vehicle group and the total delay rate increases.
#' @seealso \code{\link{TDR_2l}}, \code{\link{TDR_thr_2l}}, \code{\link{TDR_2_i_2l}}, \code{\link{f_np_D_2l}}, \code{\link{f_w_D_2l}}
#' @examples
#' TDR_1_i_2l(v_d = 200, v_o = 800, P_TO = 0.4, side_clearance = 1.4, lane_width = 3.5)
TDR_1_i_2l <- function(v_d = NULL, v_o = NULL, P_TO = NULL, side_clearance = NULL, lane_width = NULL){
  f_w_D <- f_w_D_2l(side_clearance = side_clearance, lane_width = lane_width)
  f_np_D <- f_np_D_2l(v_d = v_d, v_o = v_o, P_TO = P_TO)
  if (is.numeric(f_w_D) == TRUE){
    if (is.numeric(f_np_D) == TRUE){
      f <- TDR_thr_2l(v_d = v_d) + f_np_D + f_w_D
    }
    else {f <- f_np_D}
  }
  else {f <- f_w_D}
  f
}
