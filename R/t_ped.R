#' Total Crossing Time of Pedestrians at Signal Crosswalks
#'
#' Total crossing time of pedestrians at signal crosswalks(seconds).
#' It follows <Formula 14-6> in KHCM(2013), p.623.
#' @param L Crosswalk length (m)
#' @param S_p Average speed of pedestrians (m/s)
#' @param N_ped Pedestrians (persons) traversed in one cycle
#' @param W_E Effective crosswalk width(m). See \code{\link{W_E_ped}}
#' @keywords total crossing time pedestrian signal crosswalk
#' @seealso \code{\link{W_E_ped}}, \code{\link{T_cross_ped}}, \code{\link{LOS_cross_ped}}
#' @details 3.2 means pedestrian start-up time(seconds).
#'     It means the time until the first and last pedestrians in the preceding platoon completely enter the crosswalk.
#' @export t_ped
#' @examples
#' t_ped(L = 30, S_p = 3.3, N_ped = 100, W_E = 3)
t_ped <- function(L = NULL, S_p = NULL, N_ped = NULL, W_E = NULL){
  if (L > 0){
    if (S_p > 0){
      if (N_ped > 0){
        if (W_E > 0){t <- 3.2 + (L / S_p) + (0.81 * N_ped / W_E)}
        else {t <- 'Error : [W_E] must be positive(m). Please check that.'}
      }
      else {t <- 'Error : [N_ped] must be positive(persons). Please check that.'}
    }
    else {t <- 'Error : [S_p] must be positive(m/s). Please check that.'}
  }
  else {t <- 'Error : [L] must be positive(m). Please check that.'}
  t
}
