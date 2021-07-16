#' Traffic Going Straight Ahead of the First Right Turn on the Shared Right Turn Lane at 3-way Signalized Intersection
#'
#' Traffic going straight ahead of the first right turn on the shared right turn lane at the 3-way signal intersection(vph).
#'     This function follows <Formula 8-15> in KHCM(2013), p.239, 244.
#' @param V_R Right Turn Traffic Volume(vph)
#' @param V_TH Straight-through traffic (vph)
#' @param C Signal cycle(seconds)
#' @param N Total number of access lanes (excluding dedicated left-turn lanes)
#' @keywords traffic going straight right turn public lane signalized intersection
#' @seealso \code{\link{lane_group_3si}}
#' @export V_RF_3si
#' @examples
#' V_RF_sh_3si(V_R = 432, V_TH = 1293, C = 182, N = 4)
V_RF_sh_3si <- function(V_R = NULL, V_TH = NULL, C = NULL, N = NULL){
  if (V_R >= 0 & V_TH >= 0){
    if (C > 0){
      if (N >= 1){vrf <- 3600 * V_TH / (C * N * V_R)}
      else {vrf <- 'Error : [N] must be positive integer. Please check that.'}
    }
    else {vrf <- 'Error : [C] must be positive(sec). Please check that.'}
  }
  else {vrf <- 'Error : [V_L], [V_TH] must be positive(vph). Please check that.'}
  vrf
}
