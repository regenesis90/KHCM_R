#' The Capacity of Multi-lane Road
#'
#' Capacity on multi-lane roads (pcph).
#'     As for the capacity of the multi-lane road,
#'     2,200 pcphpl is applied for a design speed of 100 kph
#'     and 2,000 pcphpl for a design speed of 80 kph in \code{'type1'},
#'     where continuous traffic flow conditions are secured.
#'     \code{'type2'} roads are limited by the capacity of the signal intersection.
#'     \code{'type2'}, where the signal intersection is installed, follows <Formula 6-2>.
#'     It follows <Formula 6-2> and definitions in KHCM(2013), p.144.
#' @param type Type of traffic flow in multi-lane road. Choose one from : \code{'type1'}, \code{'type2'}
#' @param design_speed If \code{type == 'type1'}, design_speed must be used(kph). Choose one from : \code{100}, \code{80}
#' @param N Number of lanes going straight at the intersection.
#' @param S Saturation flow(pcphpl).
#' @param g_c_ratio Average green time ratio
#' @keywords capacity weaving section freeway main line link
#' @seealso \link{\code{type_ml}}
#' @export capa_ml The Capacity of Multi-lane Road(pcph)
#' @examples
#' capa_ml(type = 'type1', design_speed = 80, N = 3)
#' capa_ml(type = 'type2', N = 3, S = 2000, g_c_ratio = 0.2)
capa_ml <- function(type = NULL, design_speed = NULL, N = NULL, S = NULL, g_c_ratio = NULL){
  if (type == 'type1'){
    if (design_speed == 100){cap <- 2200 * N}
    else if (design_speed == 80){cap <- 2000 * N}
    else {cap <- 'Error : [design_speed] must be one of 100 or 80. Please check that.'}
  }
  else if (type == 'type2'){
    if (N >= 1 & S > 0 & g_c_ratio >=0 & g_c_ratio < 1){cap <- N * S * g_c_ratio}
    else {cap <- 'Error : Please check that --- [N] >= 1, [S] >= 0, [g_c_ratio] >= 0, [g_c_ratio] <= 1.'}
  }
  else {cap <- 'Error : [type] must be one of [type1], [type2]. Please check that.'}
  cap
}
