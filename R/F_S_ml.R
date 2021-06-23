#' Speed Correction Factor from Traffic Light
#'
#' Traffic light speed correction factor (when the average traffic volume is less than 500 vphpl, kph)
#'     When the average traffic volume of a multi-lane road section is less than 500 vphpl,
#'     there is little effect on deceleration by traffic volume,
#'     so only the basic reduction according to the density of traffic lights needs to be corrected.
#'     It follows <Table 6-6> in KHCM(2013), p.143.
#' @param v Traffic Volume(vphpl).
#' @param g_c_ratio Average of the signal going straight in the main direction. Choose one from: \code{0.80}, \code{0.75}, \code{0.70}, \code{0.65}, \code{0.60}
#' @param tl_density Density of traffic lights(ea/km)
#' @param vehicle_type Type of vehicle. Choose one from : \code{'normal'}, \code{'small_heavy'}, \code{'middle_heavy'}, \code{'large_heavy'}
#' @keywords speed correction factor traffic light
#' @seealso \code{\link{F_V_ml}}
#' @export F_S_ml Speed Correction Factor from Traffic Light(F_S, kph)
#' @examples
#' F_S_ml(v = 499, g_c_ratio = 0.80, tl_density = 0.2, vehicle_type = 'normal')
#' F_S_ml(300, 0.7, 0.4, 'middle_heavy')
F_S_ml <- function(v = NULL, g_c_ratio = NULL, tl_density = NULL, vehicle_type = NULL){
  if (v <= 500){
    if (tl_density >= 0 & tl_density <= 0.3){
      if (vehicle_type == 'normal' | vehicle_type == 'small_heavy'){
        if (g_c_ratio == 0.80){result <- 1}
        else if (g_c_ratio == 0.75){result <- 1}
        else if (g_c_ratio == 0.70){result <- 2}
        else if (g_c_ratio == 0.65){result <- 3}
        else if (g_c_ratio == 0.60){result <- 4}
        else {result <- 'Error: [g_c_ratio] must be one of 0.80, 0.75, 0.70, 0.65, 0.60. Please check that.'}
      }
      else if (vehicle_type == 'middle_heavy' | vehicle_type == 'large_heavy'){
        if (g_c_ratio == 0.80){result <- 1}
        else if (g_c_ratio == 0.75){result <- 1}
        else if (g_c_ratio == 0.70){result <- 2}
        else if (g_c_ratio == 0.65){result <- 2}
        else if (g_c_ratio == 0.60){result <- 3}
        else {result <- 'Error: [g_c_ratio] must be one of 0.80, 0.75, 0.70, 0.65, 0.60. Please check that.'}
      }
    }
    else if (tl_density > 0.3 & tl_density <= 0.5){
      if (vehicle_type == 'normal_vehicle' | vehicle_type == 'small_heavy'){
        if (g_c_ratio == 0.80){result <- 2}
        else if (g_c_ratio == 0.75){result <- 4}
        else if (g_c_ratio == 0.70){result <- 5}
        else if (g_c_ratio == 0.65){result <- 7}
        else if (g_c_ratio == 0.60){result <- 9}
        else {result <- 'Error: [g_c_ratio] must be one of 0.80, 0.75, 0.70, 0.65, 0.60. Please check that.'}
      }
      else if (vehicle_type == 'middle_heavy' | vehicle_type == 'large_heavy'){
        if (g_c_ratio == 0.80){result <- 2}
        else if (g_c_ratio == 0.75){result <- 3}
        else if (g_c_ratio == 0.70){result <- 5}
        else if (g_c_ratio == 0.65){result <- 6}
        else if (g_c_ratio == 0.60){result <- 8}
        else {result <- 'Error: [g_c_ratio] must be one of 0.80, 0.75, 0.70, 0.65, 0.60. Please check that.'}
      }
    }
    else (result <- 'Error : [tl_density] must be >= 0 and <= 0.5(ea/km). Please check that.')
  }
  else {result <- 'Error : [v] must be >= 500(vphpl). Please check that.'}
  result
}
