#' Speed Correction Factor from Traffic Light(F_S, kph)
#'
#' It follows <Table 6-6> in KHCM(2013).
#' @param V *Numeric* Traffic Volume(vphpl). This factor is available when traffic volume is under than 500vphpl.
#' @param g_c_ratio *Numeric* Average of the signal going straight in the main direction.
#' @param traffic_light_density *Numeric* Density of traffic lights(ea/km)
#' @param vehicle_type *Categorical* Choose one from : \code{'normal_vehicle'}, \code{'heavy_vehicle'}
#' @keywords speed correction factor traffic light
#' @export F_S Speed Correction Factor from Traffic Light(F_S, kph)
#' @examples
#' F_S(V = 499, g_c_ratio = 0.80, traffic_light_density = 0.2, vehicle_type = 'normal_vehicle')
#' F_S(300, 0.7, 0.4, 'heavy_vehicle')
F_S <- function(V = NULL, g_c_ratio = NULL, traffic_light_density = NULL, vehicle_type = NULL){
  if (V <= 500){
    if (traffic_light_density >= 0 & traffic_light_density <= 0.3){
      if (vehicle_type == 'normal_vehicle'){
        if (g_c_ratio == 0.80){result <- 1}
        if (g_c_ratio == 0.75){result <- 1}
        if (g_c_ratio == 0.70){result <- 2}
        if (g_c_ratio == 0.65){result <- 3}
        if (g_c_ratio == 0.60){result <- 4}
      }
      if (vehicle_type == 'heavy_vehicle'){
        if (g_c_ratio == 0.80){result <- 1}
        if (g_c_ratio == 0.75){result <- 1}
        if (g_c_ratio == 0.70){result <- 2}
        if (g_c_ratio == 0.65){result <- 2}
        if (g_c_ratio == 0.60){result <- 3}
      }
    }
    if (traffic_light_density > 0.3 & traffic_light_density <= 0.5){
      if (vehicle_type == 'normal_vehicle'){
        if (g_c_ratio == 0.80){result <- 2}
        if (g_c_ratio == 0.75){result <- 4}
        if (g_c_ratio == 0.70){result <- 5}
        if (g_c_ratio == 0.65){result <- 7}
        if (g_c_ratio == 0.60){result <- 9}
      }
      if (vehicle_type == 'heavy_vehicle'){
        if (g_c_ratio == 0.80){result <- 2}
        if (g_c_ratio == 0.75){result <- 3}
        if (g_c_ratio == 0.70){result <- 5}
        if (g_c_ratio == 0.65){result <- 6}
        if (g_c_ratio == 0.60){result <- 8}
      }
    }
    result
  }
}
