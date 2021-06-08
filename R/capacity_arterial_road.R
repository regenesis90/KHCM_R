#' The Capacity of Arterial Road(vph)
#'
#' This function follows <Formula 12-7> in KHCM(2013) p.540
#' @param N Number of lanes
#' @param g_c_ratio Green time signal cycle ratio
#' @keywords
#' @export capacity_arterial_road
#' @examples
#' capacity_arterial_road(3, 0.2)
capacity_arterial_road <- function(N = NULL, g_c_ratio = NULL){
  1800 * N * g_c_ratio
}
