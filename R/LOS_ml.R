#' Level of Service(LOS) in Multi-lane Road
#'
#' This function decides Level of Service(LOS) of multi-lane road.
#'     It follows <Table 6-8>, <Table 6-9> in KHCM(2013), p.145-146.
#' @param type Type of traffic flow in multi-lane road. Choose one from : \code{'type1'}, \code{'type2'}
#' @param design_speed If \code{type == 'type1'}, design_speed must be used(kph). Choose one from : \code{100}, \code{80}, \code{70}
#' @param v_c_ratio V/C ratio. It is neccessary when \code{type = 'type1'}
#' @param speed It means speed(kph)
#' @param g_c_ratio Average green time ratio. It is neccessary when \code{type == 'type2'}. Choose one from : \code{0.8}, \code{0.6}, \code{0.5}
#' @param service_v Service Traffic Volume(pcphpl)
#' @keywords LOS Level of Service Density V/C ratio
#' @export LOS_ml Level of Service. \code{A}, \code{B}, \code{C}, \code{D}, \code{E}, \code{F}
#' @examples
#' LOS_ml(type = 'type1', design_speed = 100, v_c_ratio = 0.20, service_v = 500, speed = 100)
#' LOS_ml(type = 'type2', v_c_ratio = '0.74', free_speed = 70, speed = 59, g_c_ratio = 0.5, service_v = 900)
LOS_ml <- function(type = NULL, design_speed = NULL, v_c_ratio = NULL, speed = NULL, g_c_ratio = NULL, service_v = NULL){
  if (type == 'type1'){
    if (design_speed == 100){
      if (is.null(v_c_ratio) == FALSE){
        if (v_c_ratio >= 0 & v_c_ratio <= 0.27){LOS <- 'A'}
        else if (v_c_ratio > 0.27 & v_c_ratio <= 0.45){LOS <- 'B'}
        else if (v_c_ratio > 0.45 & v_c_ratio <= 0.61){LOS <- 'C'}
        else if (v_c_ratio > 0.61 & v_c_ratio <= 0.80){LOS <- 'D'}
        else if (v_c_ratio > 0.80 & v_c_ratio <= 1.00){LOS <- 'E'}
        #else if (v_c_ratio > 1.00){LOS <- 'F'}
        else (LOS <- 'Error : [v_c_ratio] must be >= 0 & <= 1. Please check that.')
      }
      else {
        if (is.null(service_v) == FALSE){
          if (service_v >= 0 & service_v <= 600){LOS <- 'A'}
          else if (service_v > 600 & service_v <= 1000){LOS <- 'B'}
          else if (service_v > 1000 & service_v <= 1350){LOS <- 'C'}
          else if (service_v > 1350 & service_v <= 1750){LOS <- 'D'}
          else if (service_v > 1750 & service_v <= 2200){LOS <- 'E'}
          #else if (service_v > 2200){LOS <- 'F'}
          else {LOS <- 'Error : [service_v] must be >= 0 and <= 2200. Please check that.'}
        }
        else {
          if (is.null(speed) == FALSE){
            if (speed >= 97){LOS <- 'A'}
            else if (speed >= 95 & speed < 97){LOS <- 'B'}
            else if (speed >= 93 & speed < 95){LOS <- 'C'}
            else if (speed >= 88 & speed < 93){LOS <- 'D'}
            else if (speed >= 77 & speed < 88){LOS <- 'E'}
            else {LOS <- 'Error : [speed] must be >= 77(kph). Please check that.'}
          }
          else {LOS <- 'Error : All values are null. Please check that.'}
        }
      }
    }
    else if (design_speed == 80){
      if (is.null(v_c_ratio) == FALSE){
        if (v_c_ratio >= 0 & v_c_ratio <= 0.25){LOS <- 'A'}
        else if (v_c_ratio > 0.25 & v_c_ratio <= 0.40){LOS <- 'B'}
        else if (v_c_ratio > 0.40 & v_c_ratio <= 0.58){LOS <- 'C'}
        else if (v_c_ratio > 0.58 & v_c_ratio <= 0.75){LOS <- 'D'}
        else if (v_c_ratio > 0.75 & v_c_ratio <= 1.00){LOS <- 'E'}
        #else if (v_c_ratio > 1.00){LOS <- 'F'}
        else (LOS <- 'Error : [v_c_ratio] must be >= 0 & <= 1. Please check that.')
      }
      else {
        if (is.null(service_v) == FALSE){
          if (service_v >= 0 & service_v <= 500){LOS <- 'A'}
          else if (service_v > 500 & service_v <= 800){LOS <- 'B'}
          else if (service_v > 800 & service_v <= 1150){LOS <- 'C'}
          else if (service_v > 1150 & service_v <= 1500){LOS <- 'D'}
          else if (service_v > 1500 & service_v <= 2000){LOS <- 'E'}
          #else if (service_v > 2200){LOS <- 'F'}
          else {LOS <- 'Error : [service_v] must be >= 0 and <= 2200. Please check that.'}
        }
        else {
          if (is.null(speed) == FALSE){
            if (speed >= 86){LOS <- 'A'}
            else if (speed >= 85 & speed < 86){LOS <- 'B'}
            else if (speed >= 84 & speed < 85){LOS <- 'C'}
            else if (speed >= 79 & speed < 84){LOS <- 'D'}
            else if (speed >= 67 & speed < 79){LOS <- 'E'}
            else {LOS <- 'Error : [speed] must be >= 77(kph). Please check that.'}
          }
          else {LOS <- 'Error : All values are null. Please check that.'}
        }
      }
    }
    else {LOS <- 'Error : [design_speed] must be one of 100, 80 when type == [type1]. Please check that.'}
  }
  else if (type == 'type2'){
    if (is.null(v_c_ratio) == FALSE){
      if (v_c_ratio >= 0 & v_c_ratio <= 0.20){LOS <- 'A'}
      else if (v_c_ratio > 0.20 & v_c_ratio <= 0.45){LOS <- 'B'}
      else if (v_c_ratio > 0.45 & v_c_ratio <= 0.70){LOS <- 'C'}
      else if (v_c_ratio > 0.70 & v_c_ratio <= 0.85){LOS <- 'D'}
      else if (v_c_ratio > 0.85 & v_c_ratio <= 1.00){LOS <- 'E'}
      #else if (v_c_ratio > 1.00){LOS <- 'F'}
      else (LOS <- 'Error : [v_c_ratio] must be >= 0 & <= 1. Please check that.')
    }
    else {
      if (is.null(design_speed) == FALSE){
        if (design_speed == 80){
          if (speed >= 86){LOS <- 'A'}
          else if (speed >= 84 & speed < 86){LOS <- 'B'}
          else if (speed >= 76 & speed < 84){LOS <- 'C'}
          else if (speed >= 68 & speed < 76){LOS <- 'D'}
          else if (speed >= 58 & speed < 68){LOS <- 'E'}
          else {LOS <- 'Error : [speed] must be >= 58(kph). Please check that.'}
        }
        else if (design_speed == 70){
          if (speed >= 70){LOS <- 'A'}
          else if (speed >= 68 & speed < 70){LOS <- 'B'}
          else if (speed >= 61 & speed < 68){LOS <- 'C'}
          else if (speed >= 54 & speed < 61){LOS <- 'D'}
          else if (speed >= 46 & speed < 54){LOS <- 'E'}
          else {LOS <- 'Error : [speed] must be >= 58(kph). Please check that.'}
        }
        else {LOS <- 'Error : [design_speed] must be one of 80, 70. Please check that.'}
      }
      else {
        if (is.null(service_v) == FALSE){
          if (g_c_ratio == 0.8){}
          else if (g_c_ratio == 0.6){}
          else if (g_c_ratio == 0.5){}
          else {LOS <- 'Error : [g_c_ratio] must be one of 0.8, 0.6, 0.5. Please check that.'}
        }
        else {LOS <- 'Error : All necessary values are empty. Please check that.'}
      }
    }
  }
  else (LOS <- 'Error : [type] must be one of [type1], [type2]. Please check that.')
  LOS
}
