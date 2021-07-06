#' Time-space Area of Pedestrians at Signal Crosswalks
#'
#' Time-space area of pedestrians at signal crosswalks (㎡-person)
#' It follows <Formula 14-7> in KHCM(2013), p.624
#' @param L Crosswalk length (m)
#' @param S_p Average speed of pedestrians (m/s)
#' @param WALK_FDW Effective pedestrian green time at crosswalks (seconds)
#' @param W_E Effective crosswalk width(m). See \code{\link{W_E_ped}}
#' @keywords time-space area pedestrian signal crosswalk
#' @seealso \code{\link{W_E_ped}}, \code{\link{T_cross_ped}}, \code{\link{M_cross_ped}}, \code{\link{LOS_road_ped}}
#' @export TS_cross_ped
#' @examples
#' TS_cross_ped(L = 25, W_E = 3.3, S_p = 3, WALK_FDW = 50)
TS_cross_ped <- function(L = NULL, W_E = NULL, S_p = NULL, WALK_FDW = NULL){
  if (L > 0){
    if (W_E > 0){
      if (S_p > 0){
        if (WALK_FDW > 0){ts <- L * W_E * (WALK_FDW - (L / (2 * S_p)))}
        else {ts <- 'Error : [WALK_FDW] must be positive(s). Please check that.'}
      }
      else {ts <- 'Error : [S_p] must be positive(m/s). Please check that.'}
    }
    else {ts <- 'Error : [W_E] must be positive(m). Please check that.'}
  }
  else {ts <- 'Error : [L] must be positive(m). Please check that.'}
  ts
}
