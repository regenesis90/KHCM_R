#' Level of Service in Roundabout
#'
#' Level of service(LOS) at roundabouts.
#'     To evaluate the roundabout operation effect,
#'     the average delay (seconds/largest) is used as with the signal intersection.
#'     After calculating the average delay according to the roundabout approach,
#'     the traffic volume weighted average is used to calculate the average delay at the roundabout.
#'     This function follows <Table 11-1> in KHCM(2013) p.494.
#' @param avg_d Average Delay(sec/veh)
#' @param v_c_ratio Traffic volume to capacity ratio
#' @keywords Level of Service LOS Roundabout
#' @seealso \code{\link{d_total_rab}}
#' @export LOS_rab
#' @examples
#' LOS_rab(avg_d = 43.2, v_c_ratio = 0.8)
LOS_rab<- function(avg_d = NULL, v_c_ratio = NULL){
  if (v_c_ratio >= 0 & v_c_ratio <= 1){
    if (avg_d >= 0 & avg_d < 10){los <- 'A'}
    else if (avg_d >= 10 & avg_d < 15){los <- 'B'}
    else if (avg_d >= 15 & avg_d < 25){los <- 'C'}
    else if (avg_d >= 25 & avg_d < 35){los <- 'D'}
    else if (avg_d >= 35 & avg_d < 50){los <- 'E'}
    else if (avg_d > 50){los <- 'F'}
    else {los <- 'Error : [avg_d] must be positive(sec/veh). Please check that.'}
  }
  else if (v_c_ratio > 1){los <- 'F'}
  else {los <- 'Error : [v_c_ratio] must be positive. Please check that.'}
  los
}
