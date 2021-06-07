#' Conflict traffic volume on approach i (V_c_i_roundabout, pcph)
#'
#' It follows <Formula 11-5> in KHCM(2013) p.500
#' @param V_ip1 Traffic from Approach i+1 to Approach i+1
#' @param V_ip2 Traffic volume moving from approach i+4 to each approach i+1, i+2
#' @param V_ip3 Traffic volume moving from approach i+4 to each approach i+1, i+2, i+3
#' @param V_ip4 Traffic volume moving from approach i+4 to each approach i+1, i+2, i+3, i+4
#' @export V_c_i_roundabout Conflict traffic volume on approach i (pcph)
#' @examples
V_c_i_roundabout <- function(V_ip1 = NULL, V_ip2 = NULL, V_ip3 = NULL, V_ip4 = NULL){
  V_ip1 + V_ip2 + V_ip3 + V_ip4
}
