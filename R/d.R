#' Average control delay per vehicle (d, sec/set)
#'
#' It follows <Formula 8-43>, <Formula 12-2> in KHCM(2013) p.538
#' @param d_1
#' @param d_2
#' @param d_3
#' @param PF
#' @param f_cw Pedestrian crossing signal correction factor between signal intersections. See f_cw().
#' @export d Average control delay per vehicle
#' @examples
d <- function(d_1 = NULL, d_2 = NULL, d_3 = NULL, PF = NULL, f_cw = 1){
  d_1 * PF * f_cw + d_2 + d_3
}
