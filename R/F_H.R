#' Speed Correction Factor from Hillness(F_H)
#'
#' This function calculates vertical curve curvature in multi-lane road. It follows <Table 6-4> in KHCM(2013).
#' @param H *Numeric* Hillness. Vertical Curve Curvature in Multi-lane Road(H, degree/km)
#' @param vehicle_type *Categorical* Choose one from : \code{'normal_vehicle'}, \code{'heavy_vehicle'}
#' @param speed *Numeric* Speed(kph). What speed?
#' @keywords LOS Level of Service Density V/C ratio
#' @export F_H Speed Correction Factor from Hillness. Maximum Reduced Speed(kph)
#' @examples
#' F_H(H = 30.2, vehicle_type = 'heavy_vehicle', speed = 70)
#' F_H(23, 'normal_vehicle', 87)
F_H <- function(H = NULL, vehicle_type = NULL, speed = NULL){
  if (H >= 0 & is.null(vehicle_type) == FALSE & speed >= 0){
    if (vehicle_type == 'heavy_vehicle'){
      if (H >= 0 & H <= 2){result <- 0}
      if (H > 2 & H <= 5){result <- 3}
      if (H > 5 & H <= 10){result <- 5}
      if (H > 10 & H <= 20){result <- 10}
      if (H > 20 & H <= 30){result <- 15}
      if (H > 30 & H <= 40){result <- 19}
      if (H > 40 & H <= 50){result <- 22}
      if (H > 50 & H <= 60){result <- 25}
      if (H > 60 & H <= 70){result <- 27}
      if (H > 70 & H <= 80){result <- 28}
      if (H > 80 & H <= 90){result <- 30}
    }
    if (vehicle_type == 'normal_vehicle'){
      if (H >= 0 & H <= 2){result <- 0}
      if (H > 2 & H <= 5){result <- 1}
      if (H > 5 & H <= 10){result <- 2}
      if (H > 10 & H <= 20){result <- 4}
      if (H > 20 & H <= 30){result <- 6}
      if (H > 30 & H <= 40){
        if (speed == 97){result <- 9}
        if (speed == 87){result <- 8}
        if (speed == 70){result <- 8}
      }
      if (H > 40 & H <= 50){
        if (speed == 97){result <- 11}
        if (speed == 87){result <- 10}
        if (speed == 70){result <- 10}
      }
      if (H > 50 & H <= 60){
        if (speed == 97){result <- 13}
        if (speed == 87){result <- 12}
        if (speed == 70){result <- 12}
      }
      if (H > 60 & H <= 70){
        if (speed == 97){result <- 15}
        if (speed == 87){result <- 14}
        if (speed == 70){result <- 14}
      }
      if (H > 70 & H <= 80){
        if (speed == 97){result <- 17}
        if (speed == 87){result <- 16}
        if (speed == 70){result <- 16}
      }
      if (H > 80 & H <= 90){
        if (speed == 97){result <- 19}
        if (speed == 87){result <- 18}
        if (speed == 70){result <- 18}
      }
    }
    result
  }
}
