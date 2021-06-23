#' Speed Correction Factor from Traffic Volume
#'
#' Speed correction factor according to traffic volume
#'     (when the average traffic volume exceeds 500vphpl, kph).
#'     It shows the degree of speed decrease by vehicle type according to traffic light installation density and traffic volume.
#'     When the average traffic volume of the multi-lane road section exceeds 500vphpl,
#'     additional speed correction according to the traffic volume should be added to the speed correction by \code{\link{F_S_ml}}.
#'     It follows <Table 6-7> in KHCM(2013), p.143-144.
#' @param v Traffic volume(vphpl). This factor is available when traffic volume is under than 500vphpl.
#' @param tl_density Density of traffic lights(ea/km)
#' @param vehicle_type Type of vehicle. Choose one from : \code{'normal'}, \code{'small_heavy'}, \code{'middle_heavy'}, \code{'large_heavy'}
#' @keywords speed correction factor traffic light
#' @seealso \code{\link{F_S_ml}}
#' @export F_V_ml Speed correction factor from traffic volume
#' @examples
#' F_V_ml(v = 1231, vehicle_type = 'normal', tl_density = 0.45)
#' F_V_ml(999, 'large_heavy', 0.3293)
F_V_ml <- function(v = NULL, vehicle_type = NULL, tl_density = NULL){
  if (vehicle_type == 'normal' | vehicle_type == 'small_heavy'){
    if (tl_density >= 0 & tl_density <= 0.1){
      if (v == 500){result <- 1}
      else if (v > 500 & v < 600){result <- 1}
      else if (v == 600){result <- 1}
      else if (v > 600 & v < 700){result <- 1}
      else if (v == 700){result <- 1}
      else if (v > 700 & v < 800){result <- 1 + (v - 700) * 0.01}
      else if (v == 800){result <- 2}
      else if (v > 800 & v < 900){result <- 2}
      else if (v == 900){result <- 2}
      else if (v > 900 & v < 1000){result <- 1 + (v - 900) * 0.01}
      else if (v == 1000){result <- 3}
      else if (v > 1000 & v < 1100){result <- 1 + (v - 1000) * 0.01}
      else if (v == 1100){result <- 4}
      else if (v > 1100 & v < 1200){result <- 4}
      else if (v == 1200){result <- 4}
      else if (v > 1200 & v < 1300){result <- 4 + (v - 1200) * 0.02}
      else if (v == 1300){result <- 6}
      else if (v > 1300 & v < 1400){result <- 6 + (v - 1300) * 0.02}
      else if (v == 1400){result <- 8}
      else if (v > 1400 & v < 1500){result <- 8 + (v - 1400) * 0.01}
      else if (v == 1500){result <- 9}
      else if (v > 1500 & v < 1600){result <- 9 + (v - 1500) * 0.03}
      else if (v == 1600){result <- 12}
      else if (v > 1600 & v < 1700){result <- 12 + (v - 1600) * 0.04}
      else if (v == 1700){result <- 16}
      else if (v > 1700 & v < 1800){result <- 12 + (v - 1600) * 0.04}
      else if (v == 1800){result <- 20}
      else {result <- 'Error : [v] must be >= 500(vphpl). Please check that.'}
    }
    else if (tl_density >= 0.1 & tl_density <= 0.3){
      if (v == 500){result <- 1}
      else if (v > 500 & v < 600){result <- 1}
      else if (v == 600){result <- 1}
      else if (v > 600 & v < 700){result <- 1 + (v - 600) * 0.01}
      else if (v == 700){result <- 2}
      else if (v > 700 & v < 800){result <- 2}
      else if (v == 800){result <- 2}
      else if (v > 800 & v < 900){result <- 2 + (v - 800) * 0.01}
      else if (v == 900){result <- 3}
      else if (v > 900 & v < 1000){result <- 3 + (v - 900) * 0.01}
      else if (v == 1000){result <- 4}
      else if (v > 1000 & v < 1100){result <- 4 + (v - 1000) * 0.01}
      else if (v == 1100){result <- 5}
      else if (v > 1100 & v < 1200){result <- 5 + (v - 1100) * 0.01}
      else if (v == 1200){result <- 6}
      else if (v > 1200 & v < 1300){result <- 6 + (v - 1200) * 0.02}
      else if (v == 1300){result <- 8}
      else if (v > 1300 & v < 1400){result <- 8 + (v - 1300) * 0.02}
      else if (v == 1400){result <- 10}
      else if (v > 1400 & v < 1500){result <- 10 + (v - 1400) * 0.01}
      else if (v == 1500){result <- 11}
      else if (v > 1500 & v < 1600){result <- 11 + (v - 1500) * 0.03}
      else if (v == 1600){result <- 14}
      else if (v > 1600 & v < 1700){result <- 14 + (v - 1600) * 0.05}
      else if (v == 1700){result <- 19}
      else if (v > 1700 & v < 1800){result <- 19 + (v - 1700) * 0.04}
      else if (v == 1800){result <- 23}
      else {result <- 'Error : [v] must be >= 500(vphpl). Please check that.'}
    }
    else if (tl_density >= 0.3 & tl_density <= 0.5){
      if (v == 500){result <- 1}
      else if (v > 500 & v < 600){result <- 1 + (v - 500) * 0.01}
      else if (v == 600){result <- 2}
      else if (v > 600 & v < 700){result <- 2 + (v - 600) * 0.01}
      else if (v == 700){result <- 3}
      else if (v > 700 & v < 800){result <- 3 + (v - 700) * 0.01}
      else if (v == 800){result <- 4}
      else if (v > 800 & v < 900){result <- 4 + (v - 800) * 0.01}
      else if (v == 900){result <- 5}
      else if (v > 900 & v < 1000){result <- 5 + (v - 900) * 0.01}
      else if (v == 1000){result <- 6}
      else if (v > 1000 & v < 1100){result <- 6 + (v - 1000) * 0.01}
      else if (v == 1100){result <- 7}
      else if (v > 1100 & v < 1200){result <- 7 + (v - 1100) * 0.01}
      else if (v == 1200){result <- 8}
      else if (v > 1200 & v < 1300){result <- 7 + (v - 1200) * 0.02}
      else if (v == 1300){result <- 10}
      else if (v > 1300 & v < 1400){result <- 10 + (v - 1300) * 0.02}
      else if (v == 1400){result <- 12}
      else if (v > 1400 & v < 1500){result <- 12 + (v - 1400) * 0.04}
      else if (v == 1500){result <- 16}
      else if (v > 1500 & v < 1600){result <- 16 + (v - 1500) * 0.03}
      else if (v == 1600){result <- 19}
      else if (v > 1600 & v < 1700){result <- 19 + (v - 1600) * 0.03}
      else if (v == 1700){result <- 22}
      else if (v > 1700 & v < 1800){result <- 22 + (v - 1700) * 0.02}
      else if (v == 1800){result <- 24}
      else {result <- 'Error : [v] must be >= 500(vphpl). Please check that.'}
    }
    else {result <- 'Error : [tl_density] must be >= 0 and <= 0.5. Please check that.'}
  }
  else if (vehicle_type == 'middle_heavy' | vehicle_type == 'large_heavy'){
    if (tl_density >= 0 & tl_density <= 0.1){
      if (v == 500){result <- 1}
      else if (v > 500 & v < 600){result <- 1}
      else if (v == 600){result <- 1}
      else if (v > 600 & v < 700){result <- 1 + (v - 600) * 0.01}
      else if (v == 700){result <- 2}
      else if (v > 700 & v < 800){result <- 2}
      else if (v == 800){result <- 2}
      else if (v > 800 & v < 900){result <- 2}
      else if (v == 900){result <- 2}
      else if (v > 900 & v < 1000){result <- 1 + (v - 900) * 0.01}
      else if (v == 1000){result <- 3}
      else if (v > 1000 & v < 1100){result <- 3}
      else if (v == 1100){result <- 3}
      else if (v > 1100 & v < 1200){result <- 4 + (v - 1100) * 0.01}
      else if (v == 1200){result <- 4}
      else if (v > 1200 & v < 1300){result <- 4}
      else if (v == 1300){result <- 4}
      else if (v > 1300 & v < 1400){result <- 4 + (v - 1300) * 0.01}
      else if (v == 1400){result <- 5}
      else if (v > 1400 & v < 1500){result <- 5 + (v - 1400) * 0.01}
      else if (v == 1500){result <- 6}
      else if (v > 1500 & v < 1600){result <- 6 + (v - 1500) * 0.01}
      else if (v == 1600){result <- 7}
      else if (v > 1600 & v < 1700){result <- 7 + (v - 1600) * 0.02}
      else if (v == 1700){result <- 9}
      else if (v > 1700 & v < 1800){result <- 9 + (v - 1600) * 0.01}
      else if (v == 1800){result <- 10}
      else {result <- 'Error : [v] must be >= 500(vphpl). Please check that.'}
    }
    else if (tl_density >= 0.1 & tl_density <= 0.3){
      if (v == 500){result <- 1}
      else if (v > 500 & v < 600){result <- 1 + (v - 500) * 0.01}
      else if (v == 600){result <- 2}
      else if (v > 600 & v < 700){result <- 2}
      else if (v == 700){result <- 2}
      else if (v > 700 & v < 800){result <- 2 + (v - 700) * 0.01}
      else if (v == 800){result <- 3}
      else if (v > 800 & v < 900){result <- 3}
      else if (v == 900){result <- 3}
      else if (v > 900 & v < 1000){result <- 3 + (v - 900) * 0.01}
      else if (v == 1000){result <- 4}
      else if (v > 1000 & v < 1100){result <- 4}
      else if (v == 1100){result <- 4}
      else if (v > 1100 & v < 1200){result <- 4 + (v - 1100) * 0.01}
      else if (v == 1200){result <- 5}
      else if (v > 1200 & v < 1300){result <- 5 + (v - 1200) * 0.01}
      else if (v == 1300){result <- 6}
      else if (v > 1300 & v < 1400){result <- 6 + (v - 1300) * 0.01}
      else if (v == 1400){result <- 7}
      else if (v > 1400 & v < 1500){result <- 7 + (v - 1400) * 0.01}
      else if (v == 1500){result <- 8}
      else if (v > 1500 & v < 1600){result <- 8 + (v - 1500) * 0.01}
      else if (v == 1600){result <- 9}
      else if (v > 1600 & v < 1700){result <- 9 + (v - 1600) * 0.03}
      else if (v == 1700){result <- 12}
      else if (v > 1700 & v < 1800){result <- 12 + (v - 1700) * 0.03}
      else if (v == 1800){result <- 15}
      else {result <- 'Error : [v] must be >= 500(vphpl). Please check that.'}
    }
    else if (tl_density >= 0.3 & tl_density <= 0.5){
      if (v == 500){result <- 3}
      else if (v > 500 & v < 600){result <- 3 + (v - 500) * 0.01}
      else if (v == 600){result <- 4}
      else if (v > 600 & v < 700){result <- 4}
      else if (v == 700){result <- 4}
      else if (v > 700 & v < 800){result <- 4 + (v - 700) * 0.01}
      else if (v == 800){result <- 5}
      else if (v > 800 & v < 900){result <- 5 + (v - 800) * 0.01}
      else if (v == 900){result <- 6}
      else if (v > 900 & v < 1000){result <- 6 + (v - 900) * 0.01}
      else if (v == 1000){result <- 7}
      else if (v > 1000 & v < 1100){result <- 7 + (v - 1000) * 0.01}
      else if (v == 1100){result <- 8}
      else if (v > 1100 & v < 1200){result <- 8 + (v - 1100) * 0.01}
      else if (v == 1200){result <- 9}
      else if (v > 1200 & v < 1300){result <- 9 + (v - 1200) * 0.01}
      else if (v == 1300){result <- 10}
      else if (v > 1300 & v < 1400){result <- 10 + (v - 1300) * 0.02}
      else if (v == 1400){result <- 12}
      else if (v > 1400 & v < 1500){result <- 12 + (v - 1400) * 0.02}
      else if (v == 1500){result <- 14}
      else if (v > 1500 & v < 1600){result <- 14 + (v - 1500) * 0.03}
      else if (v == 1600){result <- 17}
      else if (v > 1600 & v < 1700){result <- 17 + (v - 1600) * 0.03}
      else if (v == 1700){result <- 20}
      else if (v > 1700 & v < 1800){result <- 20 + (v - 1700) * 0.04}
      else if (v == 1800){result <- 24}
      else {result <- 'Error : [v] must be >= 500(vphpl). Please check that.'}
    }
    else {result <- 'Error : [tl_density] must be >= 0 and <= 0.5. Please check that.'}
  }
  else {result <- 'Error : [vehicle_type] must be one of [normal], [small_heavy], [middle_heavy], [large_heavy]. Please check that.'}
  result
}
