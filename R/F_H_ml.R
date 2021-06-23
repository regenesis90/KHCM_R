#' Speed Correction Factor from Hillness(F_H)
#'
#' This function calculates vertical curve curvature in multi-lane road. It follows <Table 6-4> in KHCM(2013).
#' @param H Hillness. Vertical Curve Curvature in Multi-lane Road(H, degree/km)
#' @param vehicle_type Type of vehicle. Choose one from : \code{'normal'}, \code{'small_heavy'}, \code{'middle_heavy'}, \code{'large_heavy'}
#' @param BSP Maximum travel speed under basic conditions in multi-lane road(kph). Choose from one : \code{97}, \code{87}, \code{70}. See \code{\link{B_SP_ml}}
#' @keywords LOS Level of Service Density V/C ratio
#' @export F_H_ml BSP speed Correction Factor from Hillness. Maximum Reduced BSP(kph)
#' @examples
#' F_H_ml(H = 30.2, vehicle_type = 'middle_heavy', BSP = 70)
#' F_H_ml(23, 'normal', 87)
F_H_ml <- function(H = NULL, vehicle_type = NULL, BSP = NULL){
  if (vehicle_type == 'middle_heavy' | vehicle_type == 'large_heavy'){
    if (H >= 0 & H <= 2){result <- 0}
    else if (H > 2 & H <= 5){result <- 3}
    else if (H > 5 & H <= 10){result <- 5}
    else if (H > 10 & H <= 20){result <- 10}
    else if (H > 20 & H <= 30){result <- 15}
    else if (H > 30 & H <= 40){result <- 19}
    else if (H > 40 & H <= 50){result <- 22}
    else if (H > 50 & H <= 60){result <- 25}
    else if (H > 60 & H <= 70){result <- 27}
    else if (H > 70 & H <= 80){result <- 28}
    else if (H > 80 & H <= 90){result <- 30}
    else {result <- 'Error: [H] must be >= 0 and <= 90. Please check that.'}
  }
  else if (vehicle_type == 'normal' | vehicle_type == 'small_heayv'){
    if (BSP == 97){
      if (H >= 0 & H <= 2){result <- 0}
      else if (H > 2 & H <= 5){result <- 1}
      else if (H > 5 & H <= 10){result <- 2}
      else if (H > 10 & H <= 20){result <- 4}
      else if (H > 20 & H <= 30){result <- 6}
      else if (H > 30 & H <= 40){result <- 9}
      else if (H > 40 & H <= 50){result <- 11}
      else if (H > 50 & H <= 60){result <- 13}
      else if (H > 60 & H <= 70){result <- 15}
      else if (H > 70 & H <= 80){result <- 17}
      else if (H > 80 & H <= 90){result <- 19}
      else {result <- 'Error: [H] must be >= 0 and <= 90. Please check that.'}
    }
    else if (BSP == 87 | BSP == 70){
      if (H >= 0 & H <= 2){result <- 0}
      else if (H > 2 & H <= 5){result <- 1}
      else if (H > 5 & H <= 10){result <- 2}
      else if (H > 10 & H <= 20){result <- 4}
      else if (H > 20 & H <= 30){result <- 6}
      else if (H > 30 & H <= 40){result <- 8}
      else if (H > 40 & H <= 50){result <- 10}
      else if (H > 50 & H <= 60){result <- 12}
      else if (H > 60 & H <= 70){result <- 14}
      else if (H > 70 & H <= 80){result <- 16}
      else if (H > 80 & H <= 90){result <- 18}
      else {result <- 'Error: [H] must be >= 0 and <= 90. Please check that.'}
    }
    else {'Error : [BSP] must be one of 97, 87, 70. Please check that.'}
  }
  else {result <- 'Error : [vehicle_type] must be one of [normal], [small_heavy], [middle_heavy], [large_heavy]. Please check that.'}
  result
}
