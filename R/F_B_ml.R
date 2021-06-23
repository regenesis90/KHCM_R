#' Bendiness Speed Correction Factor
#'
#' This function calculates horizontal curve curvature in multi-lane road(kph).
#'     It means the degree of reduction in the maximum travel speed (kph) according to the flat linear curve (/km)
#'     It follows <Table 6-3> in KHCM(2013), p.141.
#' @param B Bendiness. Horizontal curve curvature in multi-lane road(degree/km). See \code{\link{B_ml}}
#' @param BSP Maximum travel BSP under basic conditions in multi-lane road(kph). Choose from one : \code{97}, \code{87}, \code{70}. See \code{\link{B_SP_ml}}
#' @keywords Bendiness Speed Correction Factor
#' @export F_B Bendiness speed correction factor(kph).
#' @examples
#' F_B_ml(B = 15, BSP = 70)
#' F_B_ml(30, 87)
F_B_ml <- function(B = NULL, BSP = NULL){
  if (BSP == 97){
    if (B >= 0 & B <= 10){result <- 0}
    else if (B > 10 & B <= 20){result <- 1}
    else if (B > 20 & B <= 40){result <- 2}
    else if (B > 40 & B <= 60){result <- 3}
    else if (B > 60 & B <= 80){result <- 4}
    else {result <- 'Error : [B] must be <= 80 and >= 0. Please check that.'}
  }
  else if (BSP == 87 | BSP == 70){
    if (B >= 0 & B <= 10){result <- 0}
    else if (B > 10 & B <= 20){result <- 1}
    else if (B > 20 & B <= 40){result <- 1}
    else if (B > 40 & B <= 60){result <- 2}
    else if (B > 60 & B <= 80){result <- 3}
    else {result <- 'Error : [B] must be <= 80 and >= 0. Please check that.'}
  }
  else {result <- 'Error : [BSP] must be one of 97, 87, 70. Please check that.'}
  result
}
