#' Roundabout Ramp Capacity
#'
#' Roundabout ramp capacity analysis model. Conflict traffic volume, driver characteristics,
#'     geometric characteristics, and number of pedestrians are reflected
#'     It follows <Formula 11-2>, <Table 11-2>, <Table 11-8> in KHCM(2013), p.496, 497, 507.
#' @param lane Number of roundabout rotation lane. Choose one from: \code{1}, \code{2}
#' @param lane_entry Number of roundabout entry lane. Choose one from: \code{1}, \code{2}
#' @param v_c Conflict traffic volume(pcph). See \code{\link{v_c_NB_rab}}
#' @param p_v Amount of passenger (person/hour)
#' @keywords roundabout ramp capacity
#' @seealso \code{\link{v_c_NB_rab}}, \code{\link{f_p_rab}}
#' @export capa_rab
#' @examples
#' capa_rab(lane = 2, lane_entry = 1, v_c = 382, p_v = 482)
capa_rab <- function(lane = NULL, lane_entry = NULL, v_c = NULL, p_v = NULL){
  if (lane == 1){
    t_c <- 3.21
    t_f <- 3.15
    t_min <- 2.05
    n_e <- 1
    if (v_c >= 0 & p_v >= 0){
      f_p <- f_p_rab(lane = lane, v_c = v_c, p_v = p_v)
      c <- 3600 * (1- (v_c * t_min / 3600)) * (n_e/t_f) * exp(-1 * (v_c/3600) * (t_c - 0.5 * t_f - t_min)) * f_p
    }
    else {c <- 'Error : [v_c], [p_v] must be positive. Please check that.'}
  }
  else if (lane == 2){
    if (lane_entry == 2){
      t_c <- 3.21
      t_f <- 3.15
      t_min <- 0
      n_e <- 1.7
      if (v_c >= 0 & p_v >= 0){
        f_p <- f_p_rab(lane = lane, v_c = v_c, p_v = p_v)
        c <- 3600 * (1- (v_c * t_min / 3600)) * (n_e/t_f) * exp(-1 * (v_c/3600) * (t_c - 0.5 * t_f - t_min)) * f_p
      }
      else {c <- 'Error : [v_c], [p_v] must be positive. Please check that.'}
    }
    else if (lane_entry == 1){
      t_c <- 3.21
      t_f <- 3.15
      t_min <- 0
      n_e <- 1
      if (v_c >= 0 & p_v >= 0){
        f_p <- f_p_rab(lane = lane, v_c = v_c, p_v = p_v)
        c <- 3600 * (1- (v_c * t_min / 3600)) * (n_e/t_f) * exp(-1 * (v_c/3600) * (t_c - 0.5 * t_f - t_min)) * f_p
      }
      else {c <- 'Error : [v_c], [p_v] must be positive. Please check that.'}
    }
    else {c <- 'Error : [lane_entry] must be one of 1, 2. Please check that.'}
  }
  else {c <- 'Error : [lane] must be one of 1, 2. Please check that.'}
  c
}
