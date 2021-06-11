#' Total crosswalk occupancy time (in-sec)
#'
#' It follows <Formula 14-7> in KHCM(2013), p.624
#' @param V_i Number of pedestrians crossing each direction (persons)
#' @param V_o Number of pedestrians crossing each direction (persons)
#' @param t Total traversing time (seconds)
#' @keywords
#' @export T_pedestrian_occupancy
#' @examples
T_pedestrian_occupancy <- function(V_i = NULL, V_o = NULL, t = NULL){
  (V_i + V_o) / t
}
