#' Saturated Traffic Volume Correction Factor for Dedicated Right Turn Lanes at Y-type and T-type 3-way Signalized Intersections
#'
#' Saturated traffic volume correction factor for dedicated right turn lanes at Y-type and T-type 3-way signalized intersections
#'     This function follows <Formula 8-37> in KHCM(2013), p.247
#' @param traffic_island Is there a right turn traffic island? Choose one from: \code{'yes'}, \code{'no'}
#' @param f_c Percentage of crosswalk signal times that are not available to vehicles turning right at a signalized intersection. See \code{\link{f_c_si}}
#' @param G_P Pedestrian signal time (sec)
#' @param L_H Friction by roadside lane going straight (seconds). See \code{\link{L_H_si}}
#' @param C Signal period length (sec)
#' @param N_R Number of lanes for dedicated right turns
#' @keywords right turn correction factor shared lane signalized intersection
#' @seealso \code{\link{f_c_si}}, \code{\link{L_H_si}}
#' @export f_RT_d_3si
#' @examples
#' f_RT_d_3si(traffic_island = 'yes', f_c = 2.3, G_P = 60, L_H = 4.2, C = 180, N_R = 2)
#' f_RT_d_3si(traffic_island = 'no', L_H = 1.2, N_R = 1)
f_RT_d_3si <- function(traffic_island = NULL, f_c = NULL, G_P = NULL, L_H = NULL, C = NULL, N_R = NULL){
  if (traffic_island == 'yes'){
    if (f_c > 0){
      if (G_P > 0 & C > 0){
        if (L_H >= 0){
          if (N_R >= 1){frt <- 0.86 * (1 - (f_c * G_P / (C * N_R)) - (L_H / (3600 * N_R)))
          }
          else {frt <- 'Error : [N_R] must be positive integer. Please check that.'}
        }
        else {frt <- 'Error : [L_H] must be positive(sec). Please check that.'}
      }
      else {frt <- 'Error : [G_P], [C] must be positive(sec). Please check that.'}
    }
    else {frt <- 'Error : [f_c] must be positive. Please check that.'}
    frt
  }
  else if (traffic_island == 'no'){
    if (L_H >= 0){
      if (N_R >= 1){frt <- 0.86 * (1 - (L_H / (3600 * N_R)))}
      else {frt <- 'Error : [N_R] must be positive integer. Please check that.'}
    }
    else {frt <- 'Error : [L_H] must be positive(sec). Please check that.'}
  }
  else {frt <- 'Error : [traffic_island] must be one of [yes], [no]. Please check that.'}
  frt
}
