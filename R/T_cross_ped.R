#' Total Crosswalk Occupation Time of Pedestrians at Signal Crosswalks
#'
#' Total crosswalk occupation time of pedestrians at signal crosswalks(person-second)
#'     It follows <Formula 14-8> in KHCM(2013), p.624.
#' @param V_i Number of pedestrians crossing in direction (persons)
#' @param V_o Number of pedestrians crossing opposite direction (persons)
#' @param t Total traversing time (seconds)
#' @keywords total crosswalk occupation time pedestrian signal crosswalk
#' @seealso \code{\link{TS_cross_ped}}, \code{\link{M_cross_ped}}, \code{\link{LOS_road_ped}}
#' @export T_cross_ped
#' @examples
#' T_cross_ped(V_i = 132, V_o = 22, t = 40)
T_cross_ped <- function(V_i = NULL, V_o = NULL, t = NULL){
  if (V_i > 0 & V_o > 0 & t > 0){res <- (V_i + V_o) * t}
  else {res <- 'Error : [V_i], [V_o], [t] must be positive. Please check that.'}
  res
}
