#' Correction of saturated traffic flow rate due to road occupation construction near intersections(S_i_WZ)
#'
#' It follows <Formula 8-67> in KHCM(2013)
#' @param S_i_WZ
#' @param g_i Effective green time of lane group i(s)
#' @param C
#' @export c_i_WZ Capacity of car group i
#' @examples
c_i_WZ <- function(S_i_WZ = NULL, g_i = NULL, C = NULL){
  S_i_WZ * g_i / C
}
