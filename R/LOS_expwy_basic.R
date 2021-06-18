#' Level of Service(LOS) in Basic Section of Expressway.
#'
#' This function decides Level of Service(LOS) in the basic section of expressway.
#'     It follows <Table 2-1>  in KHCM(2013), p.20.
#' @param density The density of the road(pcpkmpl)
#' @param design_speed Design speed(kph). Choose one from : \code{120}, \code{100}, \code{80}
#' @param v Traffic volume(pcphpl)
#' @param v_c_ratio Ratio of passing traffic volume to capacity.
#' @keywords LOS Level of Service
#' @export LOS Level of Service. \code{A}, \code{B}, \code{C}, \code{D}, \code{E}, \code{F}
#' @examples
#' LOS_expwy_basic(density = 30)
#' LOS_expwy_basic(design_speed = 120, v_c_ratio = 0.5)
#' LOS_expwy_basic(design_speed = 80, v = 1000)
LOS_expwy_basic <- function(density = NULL, design_speed = NULL, v = NULL, v_c_ratio = NULL){
  if (is.null(density) == FALSE){
    if (density >= 0 & density <= 6){LOS <- 'A'}
    else if (density > 6 & density <= 10){LOS <- 'B'}
    else if (density > 10 & density <= 14){LOS <- 'C'}
    else if (density > 14 & density <= 19){LOS <- 'D'}
    else if (density > 19 & density <= 28){LOS <- 'E'}
    else if (density > 28){LOS <- 'F'}
    else {LOS <- 'Error : [density] must be positive(pcpkmpl). Please check that.'}
  }
  else {
    if (is.null(v) == FALSE){
      if (design_speed == 120){
        if(v >= 0 & v <= 700){LOS <- 'A'}
        else if(v > 700 & v <= 1150){LOS <- 'B'}
        else if(v > 1150 & v <= 1500){LOS <- 'C'}
        else if(v > 1500 & v <= 1900){LOS <- 'D'}
        else if(v > 1900 & v <= 2300){LOS <- 'E'}
        else (LOS <- '[v] is out of range(0 <= v <= 2300). Please check that.')
      }
      else if (design_speed == 100){
        if(v >= 0 & v <= 600){LOS <- 'A'}
        else if(v > 600 & v <= 1000){LOS <- 'B'}
        else if(v > 1000 & v <= 1350){LOS <- 'C'}
        else if(v > 1350 & v <= 1750){LOS <- 'D'}
        else if(v > 1750 & v <= 2200){LOS <- 'E'}
        else (LOS <- '[v] is out of range(0 <= v <= 2200). Please check that.')
      }
      else if (design_speed == 80){
        if(v >= 0 & v <= 500){LOS <- 'A'}
        else if(v > 500 & v <= 800){LOS <- 'B'}
        else if(v > 800 & v <= 1150){LOS <- 'C'}
        else if(v > 1150 & v <= 1500){LOS <- 'D'}
        else if(v > 1500 & v <= 2000){LOS <- 'E'}
        else (LOS <- '[v] is out of range(0 <= v <= 2000). Please check that.')
      }
      else {LOS <- 'Error : [design_speed] must be one of [80], [100], [120]. Please check that.'}
      }
    else{
      if (design_speed == 120){
        if(v_c_ratio >= 0 & v_c_ratio <= 0.30){LOS <- 'A'}
        else if(v_c_ratio > 0.30 & v_c_ratio <= 0.50){LOS <- 'B'}
        else if(v_c_ratio > 0.50 & v_c_ratio <= 0.65){LOS <- 'C'}
        else if(v_c_ratio > 0.65 & v_c_ratio <= 0.83){LOS <- 'D'}
        else if(v_c_ratio > 0.83 & v_c_ratio <= 1.00){LOS <- 'E'}
        else {LOS <- 'Error : [v_c_ratio] is out of range(0 <= v_c_ratio <= 1). Please check that.'}
      }
      else if (design_speed == 100){
        if(v_c_ratio >= 0 & v_c_ratio <= 0.27){LOS <- 'A'}
        else if(v_c_ratio > 0.27 & v_c_ratio <= 0.45){LOS <- 'B'}
        else if(v_c_ratio > 0.45 & v_c_ratio <= 0.61){LOS <- 'C'}
        else if(v_c_ratio > 0.61 & v_c_ratio <= 0.80){LOS <- 'D'}
        else if(v_c_ratio > 0.80 & v_c_ratio <= 1.00){LOS <- 'E'}
        else {LOS <- 'Error : [v_c_ratio] is out of range(0 <= v_c_ratio <= 1). Please check that.'}
      }
      else if (design_speed == 80){
        if(v_c_ratio >= 0 & v_c_ratio <= 0.25){LOS <- 'A'}
        else if(v_c_ratio > 0.25 & v_c_ratio <= 0.40){LOS <- 'B'}
        else if(v_c_ratio > 0.40 & v_c_ratio <= 0.58){LOS <- 'C'}
        else if(v_c_ratio > 0.58 & v_c_ratio <= 0.75){LOS <- 'D'}
        else if(v_c_ratio > 0.75 & v_c_ratio <= 1.00){LOS <- 'E'}
        else {LOS <- 'Error : [v_c_ratio] is out of range(0 <= v_c_ratio <= 1). Please check that.'}
      }
      else {LOS <- 'Error : [design_speed] must be one of [80], [100], [120]. Please check that.'}
      }
  }
  LOS
}
