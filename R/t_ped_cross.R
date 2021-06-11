#' Total crossing time of pedestrians (seconds)
#'
#' It follows <Formula 14-6> in KHCM(2013), p.623
#'     3.2 means pedestrian start-up time(seconds).
#'     It means the Time until the first and last pedestrians in the preceding platoon completely enter the crosswalk.
#' @param L Crosswalk length (m)
#' @param S_p Average speed of pedestrians (m/s)
#' @param N_ped Pedestrians (persons) traversed in one cycle
#' @param W_E Effective crosswalk width(m). See W_E()
#' @keywords
#' @export t_ped_cross
#' @examples
t_ped_cross <- function(L = NULL, S_p = NULL, N_ped = NULL, W_E = NULL){
  3.2 + (L / S_p) + (0.81 * N_ped / W_E)
}
