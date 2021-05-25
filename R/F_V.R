#' Speed Correction Factor from Traffic Volume(F_V, kph)
#'
#' It follows <Table 6-7> in KHCM(2013).
#' @param V *Numeric* Traffic Volume(vphpl). This factor is available when traffic volume is under than 500vphpl.
#' @param traffic_light_density *Numeric* Density of traffic lights(ea/km)
#' @param vehicle_type *Categorical* Choose one from : \code{'normal_vehicle'}, \code{'heavy_vehicle'}
#' @keywords speed correction factor traffic light
#' @export F_S Speed Correction Factor from Traffic Light(F_S, kph)
#' @examples
#' F_V(V = 1231, vehicle_type = 'normal_vehicle', traffic_light_density = 0.45)
#' F_V(999, 'heavy_vehicle', 0.3293)
F_V <- function(V = NULL, vehicle_type = NULL, traffic_light_density = NULL){
  if (V >= 500){
    if (vehicle_type == 'normal_vehicle'){
      if (traffic_light_density >= 0 & traffic_light_density <= 0.1){
        if (V == 500){result <- 1}
        if (V > 500 & V < 600){result <- 1}
        if (V == 600){result <- 1}
        if (V > 600 & V < 700){result <- 1}
        if (V == 700){result <- 1}
        if (V > 700 & V < 800){result <- 1 + (V - 700) * 0.01}
        if (V == 800){result <- 2}
        if (V > 800 & V < 900){result <- 2}
        if (V == 900){result <- 2}
        if (V > 900 & V < 1000){result <- 1 + (V - 900) * 0.01}
        if (V == 1000){result <- 3}
        if (V > 1000 & V < 1100){result <- 1 + (V - 1000) * 0.01}
        if (V == 1100){result <- 4}
        if (V > 1100 & V < 1200){result <- 4}
        if (V == 1200){result <- 4}
        if (V > 1200 & V < 1300){result <- 4 + (V - 1200) * 0.02}
        if (V == 1300){result <- 6}
        if (V > 1300 & V < 1400){result <- 6 + (V - 1300) * 0.02}
        if (V == 1400){result <- 8}
        if (V > 1400 & V < 1500){result <- 8 + (V - 1400) * 0.01}
        if (V == 1500){result <- 9}
        if (V > 1500 & V < 1600){result <- 9 + (V - 1500) * 0.03}
        if (V == 1600){result <- 12}
        if (V > 1600 & V < 1700){result <- 12 + (V - 1600) * 0.04}
        if (V == 1700){result <- 16}
        if (V > 1700 & V < 1800){result <- 12 + (V - 1600) * 0.04}
        if (V == 1800){result <- 20}
      }
      if (traffic_light_density >= 0.1 & traffic_light_density <= 0.3){
        if (V == 500){result <- 1}
        if (V > 500 & V < 600){result <- 1}
        if (V == 600){result <- 1}
        if (V > 600 & V < 700){result <- 1 + (V - 600) * 0.01}
        if (V == 700){result <- 2}
        if (V > 700 & V < 800){result <- 2}
        if (V == 800){result <- 2}
        if (V > 800 & V < 900){result <- 2 + (V - 800) * 0.01}
        if (V == 900){result <- 3}
        if (V > 900 & V < 1000){result <- 3 + (V - 900) * 0.01}
        if (V == 1000){result <- 4}
        if (V > 1000 & V < 1100){result <- 4 + (V - 1000) * 0.01}
        if (V == 1100){result <- 5}
        if (V > 1100 & V < 1200){result <- 5 + (V - 1100) * 0.01}
        if (V == 1200){result <- 6}
        if (V > 1200 & V < 1300){result <- 6 + (V - 1200) * 0.02}
        if (V == 1300){result <- 8}
        if (V > 1300 & V < 1400){result <- 8 + (V - 1300) * 0.02}
        if (V == 1400){result <- 10}
        if (V > 1400 & V < 1500){result <- 10 + (V - 1400) * 0.01}
        if (V == 1500){result <- 11}
        if (V > 1500 & V < 1600){result <- 11 + (V - 1500) * 0.03}
        if (V == 1600){result <- 14}
        if (V > 1600 & V < 1700){result <- 14 + (V - 1600) * 0.05}
        if (V == 1700){result <- 19}
        if (V > 1700 & V < 1800){result <- 19 + (V - 1700) * 0.04}
        if (V == 1800){result <- 23}
      }
      if (traffic_light_density >= 0.3 & traffic_light_density <= 0.5){
        if (V == 500){result <- 1}
        if (V > 500 & V < 600){result <- 1 + (V - 500) * 0.01}
        if (V == 600){result <- 2}
        if (V > 600 & V < 700){result <- 2 + (V - 600) * 0.01}
        if (V == 700){result <- 3}
        if (V > 700 & V < 800){result <- 3 + (V - 700) * 0.01}
        if (V == 800){result <- 4}
        if (V > 800 & V < 900){result <- 4 + (V - 800) * 0.01}
        if (V == 900){result <- 5}
        if (V > 900 & V < 1000){result <- 5 + (V - 900) * 0.01}
        if (V == 1000){result <- 6}
        if (V > 1000 & V < 1100){result <- 6 + (V - 1000) * 0.01}
        if (V == 1100){result <- 7}
        if (V > 1100 & V < 1200){result <- 7 + (V - 1100) * 0.01}
        if (V == 1200){result <- 8}
        if (V > 1200 & V < 1300){result <- 7 + (V - 1200) * 0.02}
        if (V == 1300){result <- 10}
        if (V > 1300 & V < 1400){result <- 10 + (V - 1300) * 0.02}
        if (V == 1400){result <- 12}
        if (V > 1400 & V < 1500){result <- 12 + (V - 1400) * 0.04}
        if (V == 1500){result <- 16}
        if (V > 1500 & V < 1600){result <- 16 + (V - 1500) * 0.03}
        if (V == 1600){result <- 19}
        if (V > 1600 & V < 1700){result <- 19 + (V - 1600) * 0.03}
        if (V == 1700){result <- 22}
        if (V > 1700 & V < 1800){result <- 22 + (V - 1700) * 0.02}
        if (V == 1800){result <- 24}
      }
    }
    if (vehicle_type == 'heavy_vehicle'){
      if (traffic_light_density >= 0 & traffic_light_density <= 0.1){
        if (V == 500){result <- 1}
        if (V > 500 & V < 600){result <- 1}
        if (V == 600){result <- 1}
        if (V > 600 & V < 700){result <- 1 + (V - 600) * 0.01}
        if (V == 700){result <- 2}
        if (V > 700 & V < 800){result <- 2}
        if (V == 800){result <- 2}
        if (V > 800 & V < 900){result <- 2}
        if (V == 900){result <- 2}
        if (V > 900 & V < 1000){result <- 1 + (V - 900) * 0.01}
        if (V == 1000){result <- 3}
        if (V > 1000 & V < 1100){result <- 3}
        if (V == 1100){result <- 3}
        if (V > 1100 & V < 1200){result <- 4 + (V - 1100) * 0.01}
        if (V == 1200){result <- 4}
        if (V > 1200 & V < 1300){result <- 4}
        if (V == 1300){result <- 4}
        if (V > 1300 & V < 1400){result <- 4 + (V - 1300) * 0.01}
        if (V == 1400){result <- 5}
        if (V > 1400 & V < 1500){result <- 5 + (V - 1400) * 0.01}
        if (V == 1500){result <- 6}
        if (V > 1500 & V < 1600){result <- 6 + (V - 1500) * 0.01}
        if (V == 1600){result <- 7}
        if (V > 1600 & V < 1700){result <- 7 + (V - 1600) * 0.02}
        if (V == 1700){result <- 9}
        if (V > 1700 & V < 1800){result <- 9 + (V - 1600) * 0.01}
        if (V == 1800){result <- 10}
      }
      if (traffic_light_density >= 0.1 & traffic_light_density <= 0.3){
        if (V == 500){result <- 1}
        if (V > 500 & V < 600){result <- 1 + (V - 500) * 0.01}
        if (V == 600){result <- 2}
        if (V > 600 & V < 700){result <- 2}
        if (V == 700){result <- 2}
        if (V > 700 & V < 800){result <- 2 + (V - 700) * 0.01}
        if (V == 800){result <- 3}
        if (V > 800 & V < 900){result <- 3}
        if (V == 900){result <- 3}
        if (V > 900 & V < 1000){result <- 3 + (V - 900) * 0.01}
        if (V == 1000){result <- 4}
        if (V > 1000 & V < 1100){result <- 4}
        if (V == 1100){result <- 4}
        if (V > 1100 & V < 1200){result <- 4 + (V - 1100) * 0.01}
        if (V == 1200){result <- 5}
        if (V > 1200 & V < 1300){result <- 5 + (V - 1200) * 0.01}
        if (V == 1300){result <- 6}
        if (V > 1300 & V < 1400){result <- 6 + (V - 1300) * 0.01}
        if (V == 1400){result <- 7}
        if (V > 1400 & V < 1500){result <- 7 + (V - 1400) * 0.01}
        if (V == 1500){result <- 8}
        if (V > 1500 & V < 1600){result <- 8 + (V - 1500) * 0.01}
        if (V == 1600){result <- 9}
        if (V > 1600 & V < 1700){result <- 9 + (V - 1600) * 0.03}
        if (V == 1700){result <- 12}
        if (V > 1700 & V < 1800){result <- 12 + (V - 1700) * 0.03}
        if (V == 1800){result <- 15}
      }
      if (traffic_light_density >= 0.3 & traffic_light_density <= 0.5){
        if (V == 500){result <- 3}
        if (V > 500 & V < 600){result <- 3 + (V - 500) * 0.01}
        if (V == 600){result <- 4}
        if (V > 600 & V < 700){result <- 4}
        if (V == 700){result <- 4}
        if (V > 700 & V < 800){result <- 4 + (V - 700) * 0.01}
        if (V == 800){result <- 5}
        if (V > 800 & V < 900){result <- 5 + (V - 800) * 0.01}
        if (V == 900){result <- 6}
        if (V > 900 & V < 1000){result <- 6 + (V - 900) * 0.01}
        if (V == 1000){result <- 7}
        if (V > 1000 & V < 1100){result <- 7 + (V - 1000) * 0.01}
        if (V == 1100){result <- 8}
        if (V > 1100 & V < 1200){result <- 8 + (V - 1100) * 0.01}
        if (V == 1200){result <- 9}
        if (V > 1200 & V < 1300){result <- 9 + (V - 1200) * 0.01}
        if (V == 1300){result <- 10}
        if (V > 1300 & V < 1400){result <- 10 + (V - 1300) * 0.02}
        if (V == 1400){result <- 12}
        if (V > 1400 & V < 1500){result <- 12 + (V - 1400) * 0.02}
        if (V == 1500){result <- 14}
        if (V > 1500 & V < 1600){result <- 14 + (V - 1500) * 0.03}
        if (V == 1600){result <- 17}
        if (V > 1600 & V < 1700){result <- 17 + (V - 1600) * 0.03}
        if (V == 1700){result <- 20}
        if (V > 1700 & V < 1800){result <- 20 + (V - 1700) * 0.04}
        if (V == 1800){result <- 24}
      }
    }
    result
  }
}
