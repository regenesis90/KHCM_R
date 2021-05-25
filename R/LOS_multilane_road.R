#' Level of Service(LOS) in Multi-lane Road
#'
#' This function decides Level of Service(LOS). It follows <Table 6-8>
#' @param type *Categorical* Type of traffic flow in multi-lane road. Choose one from : \code{'interrupted_flow'}, \code{'uninterrupted_flow'}
#' @param design_speed *Categorical* If \code{type == 'uninterrupted_flow'}, design_speed must be used.
#' @param v_c_ratio *Numeric* V/C ratio. It is neccessary when \code{type = 'uninterrupted_flow'}
#' @param free_speed *Numeric* If \code{type == 'interrupted_flow'}, It means free-speed(kph).
#' @param speed *Numeric* It means speed(kph)
#' @param g_c_ratio *Numeric* Average green time ratio. It is neccessary when \code{type == 'interrupted_flow'}
#' @param service_volume *Numeric* Service Traffic Volume(pcphpl)
#' @keywords LOS Level of Service Density V/C ratio
#' @export LOS_multilane_road Level of Service. \code{A}, \code{B}, \code{C}, \code{D}, \code{E}, \code{F}
#' @examples
#' LOS_multilane_road(type = 'uninterrupted_flow', design_speed = 100, v_c_ratio = 0.20, service_volume = 500, speed = 100)
#' LOS_multilane_road(type = 'interrupted_flow', v_c_ratio = '0.74', free_speed = 70, speed = 59, g_c_ratio = 0.5, service_volume = 900)
LOS_multilane_road <- function(type = NULL, design_speed = NULL, v_c_ratio = NULL, free_speed = NULL, speed = NULL, g_c_ratio = NULL, service_volume = NULL){
  if (type == 'uninterrupted_flow'){
    if (design_speed == 100){
      if (v_c_ratio > 0 & v_c_ratio <= 0.27 & service_volume <= 600 & service_volume >= 0 & speed >= 97){LOS <- 'A'}
      if (v_c_ratio > 0.27 & v_c_ratio <= 0.45 & service_volume <= 1000 & service_volume >= 0 & speed >= 95){LOS <- 'B'}
      if (v_c_ratio > 0.45 & v_c_ratio <= 0.61 & service_volume <= 1350  & service_volume >= 0 & speed >= 93){LOS <- 'C'}
      if (v_c_ratio > 0.61 & v_c_ratio <= 0.80 & service_volume <= 1750 & service_volume >= 0 & speed >= 88){LOS <- 'D'}
      if (v_c_ratio > 0.80 & v_c_ratio <= 1.00 & service_volume <= 2200 & service_volume >= 0 & speed >= 77){LOS <- 'E'}
    }
    if (design_speed == 80){
      if (v_c_ratio > 0 & v_c_ratio <= 0.25 & service_volume <= 500 & service_volume >= 0 & speed >= 86){LOS <- 'A'}
      if (v_c_ratio > 0.25 & v_c_ratio <= 0.40 & service_volume <= 800 & service_volume >= 0 & speed >= 85){LOS <- 'B'}
      if (v_c_ratio > 0.40 & v_c_ratio <= 0.58 & service_volume <= 1150  & service_volume >= 0 & speed >= 84){LOS <- 'C'}
      if (v_c_ratio > 0.58 & v_c_ratio <= 0.75 & service_volume <= 1500 & service_volume >= 0 & speed >= 79){LOS <- 'D'}
      if (v_c_ratio > 0.75 & v_c_ratio <= 1.00 & service_volume <= 2000 & service_volume >= 0 & speed >= 67){LOS <- 'E'}
    }
  }
  if (type == 'interrupted_flow'){
    if (free_speed == 87){
      if (g_c_ratio == 0.8){
        if (v_c_ratio > 0 & v_c_ratio <= 0.20 & speed >= 86 & service_volume <= 350){LOS <- 'A'}
        if (v_c_ratio > 0.20 & v_c_ratio <= 0.45 & speed >= 84 & service_volume <= 800){LOS <- 'B'}
        if (v_c_ratio > 0.45 & v_c_ratio <= 0.70 & speed >= 76 & service_volume <= 1250){LOS <- 'C'}
        if (v_c_ratio > 0.70 & v_c_ratio <= 0.85 & speed >= 68 & service_volume <= 1500){LOS <- 'D'}
        if (v_c_ratio > 0.85 & v_c_ratio <= 1.00 & speed >= 58 & service_volume <= 1750){LOS <- 'E'}
      }
      if (g_c_ratio == 0.6){
        if (v_c_ratio > 0 & v_c_ratio <= 0.20 & speed >= 86 & service_volume <= 250){LOS <- 'A'}
        if (v_c_ratio > 0.20 & v_c_ratio <= 0.45 & speed >= 84 & service_volume <= 600){LOS <- 'B'}
        if (v_c_ratio > 0.45 & v_c_ratio <= 0.70 & speed >= 76 & service_volume <= 900){LOS <- 'C'}
        if (v_c_ratio > 0.70 & v_c_ratio <= 0.85 & speed >= 68 & service_volume <= 1100){LOS <- 'D'}
        if (v_c_ratio > 0.85 & v_c_ratio <= 1.00 & speed >= 58 & service_volume <= 1500){LOS <- 'E'}
      }
      if (g_c_ratio == 0.5){
        if (v_c_ratio > 0 & v_c_ratio <= 0.20 & speed >= 86 & service_volume <= 200){LOS <- 'A'}
        if (v_c_ratio > 0.20 & v_c_ratio <= 0.45 & speed >= 84 & service_volume <= 500){LOS <- 'B'}
        if (v_c_ratio > 0.45 & v_c_ratio <= 0.70 & speed >= 76 & service_volume <= 800){LOS <- 'C'}
        if (v_c_ratio > 0.70 & v_c_ratio <= 0.85 & speed >= 68 & service_volume <= 950){LOS <- 'D'}
        if (v_c_ratio > 0.85 & v_c_ratio <= 1.00 & speed >= 58 & service_volume <= 1100){LOS <- 'E'}
      }
    }
    if (free_speed == 70){
      if (g_c_ratio == 0.8){
        if (v_c_ratio > 0 & v_c_ratio <= 0.20 & speed >= 70 & service_volume <= 350){LOS <- 'A'}
        if (v_c_ratio > 0.20 & v_c_ratio <= 0.45 & speed >= 68 & service_volume <= 800){LOS <- 'B'}
        if (v_c_ratio > 0.45 & v_c_ratio <= 0.70 & speed >= 61 & service_volume <= 1250){LOS <- 'C'}
        if (v_c_ratio > 0.70 & v_c_ratio <= 0.85 & speed >= 54 & service_volume <= 1500){LOS <- 'D'}
        if (v_c_ratio > 0.85 & v_c_ratio <= 1.00 & speed >= 46 & service_volume <= 1750){LOS <- 'E'}
      }
      if (g_c_ratio == 0.6){
        if (v_c_ratio > 0 & v_c_ratio <= 0.20 & speed >= 70 & service_volume <= 250){LOS <- 'A'}
        if (v_c_ratio > 0.20 & v_c_ratio <= 0.45 & speed >= 68 & service_volume <= 600){LOS <- 'B'}
        if (v_c_ratio > 0.45 & v_c_ratio <= 0.70 & speed >= 61 & service_volume <= 900){LOS <- 'C'}
        if (v_c_ratio > 0.70 & v_c_ratio <= 0.85 & speed >= 54 & service_volume <= 1100){LOS <- 'D'}
        if (v_c_ratio > 0.85 & v_c_ratio <= 1.00 & speed >= 46 & service_volume <= 1500){LOS <- 'E'}
      }
      if (g_c_ratio == 0.5){
        if (v_c_ratio > 0 & v_c_ratio <= 0.20 & speed >= 70 & service_volume <= 200){LOS <- 'A'}
        if (v_c_ratio > 0.20 & v_c_ratio <= 0.45 & speed >= 68 & service_volume <= 500){LOS <- 'B'}
        if (v_c_ratio > 0.45 & v_c_ratio <= 0.70 & speed >= 61 & service_volume <= 800){LOS <- 'C'}
        if (v_c_ratio > 0.70 & v_c_ratio <= 0.85 & speed >= 54 & service_volume <= 950){LOS <- 'D'}
        if (v_c_ratio > 0.85 & v_c_ratio <= 1.00 & speed >= 46 & service_volume <= 1100){LOS <- 'E'}
      }
    }
  }
  LOS
}
