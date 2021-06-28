#' Level of Service(LOS) in 2-lane Road
#'
#' This function decides Level of Service(LOS) in 2-lane road.
#'     It follows <Table 7-2> in KHCM(2013), p.169.
#' @param type Type of the 2-lane road. Choose one from : \code{'type1'}, \code{'type2'}. See \code{\link{type_2l}}
#' @param design_speed Designed speed(kph). Choose one from : \code{100}, \code{90}, \code{80}
#' @param speed Travel peed(kph)
#' @param TDR Total delay rate(%). See \code{\link{TDR_thr_2l}}
#' @param v Bi-directional traffic volume(pcph)
#' @keywords LOS Level of Service Density V/C ratio
#' @export LOS_2l Level of Service. \code{A}, \code{B}, \code{C}, \code{D}, \code{E}, \code{F}
#' @seealso \code{\link{type_2l}}, \code{\link{TDR_thr_2l}}
#' @examples
#' LOS_2l(v = 2832)
#' LOS_2l(type = 'type1', design_speed = 80, speed = 38)
#' LOS_2l(type = 'type2', TDR = 34)
LOS_2l <- function(type = NULL, design_speed = NULL, speed = NULL, TDR = NULL, v = NULL){
  if (is.null(type) == FALSE){
    if (type == 'type1'){
      if (is.null(TDR) == FALSE){
        if (TDR >= 0 & TDR <= 11){los <- 'A'}
        else if (TDR > 11 & TDR <= 21){los <- 'B'}
        else if (TDR > 21 & TDR <= 30){los <- 'C'}
        else if (TDR > 30 & TDR <= 39){los <- 'D'}
        else if (TDR > 39 & TDR <= 48){los <- 'E'}
        else if (TDR > 48 & TDR <= 100){los <- 'F'}
        else {los <- 'Error : [TDR] must be positive(%). Please check that.'}
      }
      else {
        if (design_speed == 100){
          if (speed >= 95){los <- 'A'}
          else if (speed >= 85 & speed < 95){los <- 'B'}
          else if (speed >= 80 & speed < 85){los <- 'C'}
          else if (speed >= 75 & speed < 80){los <- 'D'}
          else if (speed >= 70 & speed < 75){los <- 'E'}
          else if (speed < 70){los <- 'F'}
          else {los <- 'Error : [speed] must be positive(kph). Please check that.'}
        }
        else if (design_speed == 90){
          if (speed >= 85){los <- 'A'}
          else if (speed >= 75 & speed < 85){los <- 'B'}
          else if (speed >= 70 & speed < 75){los <- 'C'}
          else if (speed >= 65 & speed < 70){los <- 'D'}
          else if (speed >= 60 & speed < 65){los <- 'E'}
          else if (speed < 60){los <- 'F'}
          else {los <- 'Error : [speed] must be positive(kph). Please check that.'}
        }
        else if (design_speed == 80){
          if (speed >= 75){los <- 'A'}
          else if (speed >= 65 & speed < 75){los <- 'B'}
          else if (speed >= 60 & speed < 65){los <- 'C'}
          else if (speed >= 55 & speed < 60){los <- 'D'}
          else if (speed >= 50 & speed < 55){los <- 'E'}
          else if (speed < 50){los <- 'F'}
          else {los <- 'Error : [speed] must be positive(kph). Please check that.'}
        }
        else {los <- 'Error : [design_speed] must be one of 100, 90, 80. Please check that'}
      }
    }
    else if (type == 'type2'){
      if (is.null(TDR) == FALSE){
        if (TDR >= 0 & TDR <= 15){los <- 'A'}
        else if (TDR > 15 & TDR <= 25){los <- 'B'}
        else if (TDR > 25 & TDR <= 40){los <- 'C'}
        else if (TDR > 40 & TDR <= 50){los <- 'D'}
        else if (TDR > 50 & TDR <= 60){los <- 'E'}
        else if (TDR > 60 & TDR <= 100){los <- 'F'}
        else {los <- 'Error : [TDR] must be positive(%). Please check that.'}
      }
      else {
        if (design_speed == 70){
          if (speed >= 65){los <- 'A'}
          else if (speed >= 55 & speed < 65){los <- 'B'}
          else if (speed >= 45 & speed < 55){los <- 'C'}
          else if (speed >= 40 & speed < 45){los <- 'D'}
          else if (speed >= 35 & speed < 40){los <- 'E'}
          else if (speed < 35){los <- 'F'}
          else {los <- 'Error : [speed] must be positive(kph). Please check that.'}
        }
        else if (design_speed == 60){
          if (speed >= 55){los <- 'A'}
          else if (speed >= 45 & speed < 55){los <- 'B'}
          else if (speed >= 40 & speed < 45){los <- 'C'}
          else if (speed >= 30 & speed < 40){los <- 'D'}
          else if (speed >= 25 & speed < 30){los <- 'E'}
          else if (speed < 25){los <- 'F'}
          else {los <- 'Error : [speed] must be positive(kph). Please check that.'}
        }
        else {los <- 'Error : [design_speed] must be one of 70, 60. Please check that'}
      }
    }
    else {los <- 'Error : [type] must be one of [type1], [type2]. Please check that.'}
  }
  else {
    if (v >= 0 & v <= 650){los <- 'A'}
    else if (v > 650 & v <= 1300){los <- 'B'}
    else if (v > 1300 & v <= 1900){los <- 'C'}
    else if (v > 1900 & v <= 2600){los <- 'D'}
    else if (v > 2600 & v <= 3200){los <- 'E'}
    else {los <- 'Error : [v] must be >= 0 and <= 3200(pcph). Please check that.'}
  }
  los
}
