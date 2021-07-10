#' Traffic Volume Entering the i-direction at Roundabouts
#'
#' Traffic volume entering the i-direction at roundabouts(pcph).
#'     It follows <Formula 11-5>, <Formula 11-6> in KHCM(2013), p.500.
#' @param right_turn_lane Is there a dedicated right-turn lane? Choose one from: \code{'yes'}, \code{'no'}
#' @param V_L i-direction left turn traffic volume(pcph)
#' @param V_T i-direction straight traffic volume(pcph)
#' @param V_R i-direction right turn traffic volume(pcph)
#' @param V_U i-direction U-turn traffic volume(pcph)
#' @keywords traffic volume direction roundabout
#' @export V_e_i_rab
#' @examples
#' V_e_i_rab(right_turn_lane = 'yes', V_L = 203, V_T = 492, V_R = 94, V_U = 11)
V_e_i_rab <- function(right_turn_lane = NULL, V_L = NULL, V_T = NULL, V_R = NULL, V_U = NULL){
  if (right_turn_lane == 'no'){
    if (V_L >= 0 & V_T >= 0 & V_R >= 0 & V_U >= 0){v <- V_L + V_T + V_R + V_U}
    else {v <- 'Error : [V_L], [V_T], [V_R], [V_U] must be >= 0(pcph). Please check that.'}
    }
  else if (right_turn_lane == 'yes'){
    if (V_L >= 0 & V_T >= 0 & V_R >= 0 & V_U >= 0){v <- V_L + V_T + V_U}
    else {v <- 'Error : [V_L], [V_T], [V_R], [V_U] must be >= 0(pcph). Please check that.'}
    }
  else {v <- 'Error : [right_turn_lane] must be one of [yes], [no]. Please check that.'}
  v
}
