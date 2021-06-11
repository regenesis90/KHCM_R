#' Pedestrian traffic flow rate (person/min/m)
#'
#' It follows <Formula 14-4> in KHCM(2013), p.620
#'     Convert the peak 15-minute pedestrian traffic surveyed to the pedestrian traffic flow rate
#' @param V_15
#' @param W_E
#' @keywords
#' @export V_pedestrian_traffic_flow pedestrian traffic flow rate (person/min/m)
#' @examples
V_pedestrian_traffic_flow <- function(V_15 = NULL, W_E = NULL){
  V_15 / (15 * W_E)
}
