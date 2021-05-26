#' Level of Service(LOS) in 2-lane Road
#'
#' This function decides Level of Service(LOS). It follows <Table 7-2>
#' @param type *Categorical* Type of the 2-lane road. Choose one from : \code{'type_1'}, \code{'type_2'}
#' @param free_speed *Categorical* kph. Choose one from : \code{100}, \code{90}, \code{80}
#' @param speed *Numeric* It means speed(kph)
#' @param TDR *Numeric* Total delay rate(%). see TDR()
#' @param V *Numeric* Traffic Volume(pcph)
#' @keywords LOS Level of Service Density V/C ratio
#' @export LOS_multilane_road Level of Service. \code{A}, \code{B}, \code{C}, \code{D}, \code{E}, \code{F}
#' @examples
#' LOS_2lane_road(type = 'type_1', free_speed = 100, speed = 74, TDR = 44, V = 2900)
#' LOS_2lane_road('type_2', 60, 45, 23, 1200)
LOS_2lane_road <- function(type = NULL, free_speed = NULL, speed = NULL, TDR = NULL, V = NULL){
  if (type == 'type_1'){
    if (free_speed == 100){
      if (V <= 650 & TDR > 0 & TDR <= 11 & speed >= 95){LOS <- 'A'}
      if (V <= 1300 & TDR > 11 & TDR <= 21 & speed < 95 & speed >= 85){LOS <- 'B'}
      if (V <= 1900 & TDR > 21 & TDR <= 30 & speed < 85 & speed >= 80){LOS <- 'C'}
      if (V <= 2600 & TDR > 30 & TDR <= 39 & speed < 80 & speed >= 75){LOS <- 'D'}
      if (V <= 3200 & TDR > 39 & TDR <= 48 & speed < 75 & speed >= 70){LOS <- 'E'}
      if (TDR > 48 & speed < 70){LOS <- 'F'}
    }
    if (free_speed == 90){
      if (V <= 650 & TDR > 0 & TDR <= 11 & speed >= 85){LOS <- 'A'}
      if (V <= 1300 & TDR > 11 & TDR <= 21 & speed >= 75){LOS <- 'B'}
      if (V <= 1900 & TDR > 21 & TDR <= 30 & speed >= 70){LOS <- 'C'}
      if (V <= 2600 & TDR > 30 & TDR <= 39 & speed >= 65){LOS <- 'D'}
      if (V <= 3200 & TDR > 39 & TDR <= 48 & speed >= 60){LOS <- 'E'}
      if (TDR > 48 & speed < 60){LOS <- 'F'}
    }
    if (free_speed == 80){
      if (V <= 650 & TDR > 0 & TDR <= 11 & speed >= 75){LOS <- 'A'}
      if (V <= 1300 & TDR > 21 & TDR <= 21 & speed >= 65){LOS <- 'B'}
      if (V <= 1900 & TDR > 30 & TDR <= 30 & speed >= 60){LOS <- 'C'}
      if (V <= 2600 & TDR > 39 & TDR <= 39 & speed >= 55){LOS <- 'D'}
      if (V <= 3200 & TDR > 48 & TDR <= 48 & speed >= 50){LOS <- 'E'}
      if (TDR > 48 & speed < 50){LOS <- 'F'}
    }
  }
  if (type == 'type_2'){
    if (free_speed == 70){
      if (V <= 650 & TDR > 0 & TDR <= 15 & speed >= 65){LOS <- 'A'}
      if (V <= 1300 & TDR > 15 & TDR <= 25 & speed >= 55){LOS <- 'B'}
      if (V <= 1900 & TDR > 25 & TDR <= 40 & speed >= 45){LOS <- 'C'}
      if (V <= 2600 & TDR > 40 & TDR <= 50 & speed >= 40){LOS <- 'D'}
      if (V <= 3200 & TDR > 50 & TDR <= 60 & speed >= 35){LOS <- 'E'}
      if (TDR > 60 & speed < 35){LOS <- 'F'}
    }
    if (free_speed == 60){
      if (V <= 650 & TDR > 0 & TDR <= 15 & speed >= 55){LOS <- 'A'}
      if (V <= 1300 & TDR > 15 & TDR <= 25 & speed >= 45){LOS <- 'B'}
      if (V <= 1900 & TDR > 25 & TDR <= 40 & speed >= 40){LOS <- 'C'}
      if (V <= 2600 & TDR > 40 & TDR <= 50 & speed >= 30){LOS <- 'D'}
      if (V <= 3200 & TDR > 60 & TDR <= 60 & speed >= 25){LOS <- 'E'}
      if (TDR > 60 & speed < 25){LOS <- 'F'}
    }
  }
  LOS
}
