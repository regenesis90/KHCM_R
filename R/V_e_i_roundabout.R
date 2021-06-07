#' i-direction entry traffic in roundabout(V_e_i_roundabout, pcph)
#'
#' It follows <Formula 11-5> in KHCM(2013) p.500
#' @param right_turn_lane Choose one from: \code{'yes'}, \code{'no'}
#' @param V_i_L i-direction left turn traffic volume(pcph)
#' @param V_i_T i-direction straight traffic volume(pcph)
#' @param V_i_R i-direction right turn traffic volume(pcph)
#' @param V_i_U i-direction U-turn traffic volume(pcph)
#' @export V_e_i_roundabout
#' @examples
V_e_i_roundabout <- function(right_turn_lane = NULL, V_i_L = NULL, V_i_T = NULL, V_i_R = NULL, V_i_U = NULL){
  if (right_turn_lane == 'no'){v <- V_i_L + V_i_T + V_i_R + V_i_U}
  if (right_turn_lane == 'yes'){v <- V_i_L + V_i_T + V_i_U}
  v
}
