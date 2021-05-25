#' Bendiness Speed Correction Factor(F_B)
#'
#' This function calculates horizontal curve curvature in multi-lane road. It follows <Table 6-3> in KHCM(2013).
#' @param B *Numeric* Bendiness. Horizontal Curve Curvature in Multi-lane Road(B, degree/km)
#' @param speed *Numeric* Speed(kph). What speed?
#' @keywords LOS Level of Service Density V/C ratio
#' @export F_B Bendiness Speed Correction Factor. Maximum Reduced Speed(kph)
#' @examples
#' F_B(B = 4, speed = 70)
#' F_B(30, 87)
F_B <- function(B = NULL, speed = NULL){
  if (speed == 97){
    if (B >= 0 & B <= 10){result <- 0}
    if (B > 10 & B <= 20){result <- 1}
    if (B > 20 & B <= 40){result <- 2}
    if (B > 40 & B <= 60){result <- 3}
    if (B > 60 & B <= 80){result <- 4}
  }
  if (speed == 87 | speed == 70){
    if (B >= 0 & B <= 10){result <- 0}
    if (B > 10 & B <= 20){result <- 1}
    if (B > 20 & B <= 40){result <- 1}
    if (B > 40 & B <= 60){result <- 2}
    if (B > 60 & B <= 80){result <- 3}
  }
  result
}
