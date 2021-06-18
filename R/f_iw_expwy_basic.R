#' Correction Factor for Capacity Estimation in Case of Bad Weather in Basic Section of Expressway
#'
#' It is a coefficient indicating the effect of bad weather on traffic flow under basic road traffic conditions.
#'     It follows <Table 2-11> in KHCM(2013), p.41.
#' @param design_speed Design speed(kph). Choose one from : \code{120}, \code{100}, \code{80}
#' @param weather Weather condition. Choose one from : \code{'sunny'}, \code{'rainy'}, \code{'snowy'}
#' @param precip Rainfall or snowfall or hail. Precipitation(mm/h).
#' @export f_iw_expwy_basic
#' @examples
#' f_iw_expwy_basic(design_speed = 80, weather = 'sunny')
#' f_iw_expwy_basic(design_speed = 100, weather = 'rainy', precip = 8)
#' f_iw_expwy_basic(120, 'snowy', 12)
f_iw_expwy_basic <- function(design_speed = NULL, weather = NULL, precip = NULL){
  if (weather == "sunny"){f_iw <- 1.00}
  else if (weather == "rainy"){
    if (design_speed == 80){
      if (precip == 0){f_iw <- 1.00}
      else if (precip > 0 & precip < 5){f_iw <- 0.97}
      else if (precip >= 5 & precip < 10){f_iw <- 0.92}
      else if (precip >= 10){f_iw <- 0.90}
      else {f_iw <- 'Error : [precip] must be positive. Please check that.'}
    }
    else if (design_speed == 100){
      if (precip == 0){f_iw <- 1.00}
      else if (precip > 0 & precip < 5){f_iw <- 0.94}
      else if (precip >= 5 & precip < 10){f_iw <- 0.90}
      else if (precip >= 10){f_iw <- 0.86}
    }
    else if (design_speed == 120){
      if (precip == 0){f_iw <- 1.00}
      else if (precip > 0 & precip < 5){f_iw <- 0.92}
      else if (precip >= 5 & precip < 10){f_iw <- 0.86}
      else if (precip >= 10){f_iw <- 0.81}
      else {f_iw <- 'Error : [precip] must be positive. Please check that.'}
    }
    else {f_iw <- 'Error : [design_speed] must be one of [80], [100], [120]. Please check that.'}
  }
  else if (weather == "snowy"){
    if (design_speed == 80){
      if (precip == 0){f_iw <- 1.00}
      if (precip > 0 & precip < 5){f_iw <- 0.87}
      if (precip >= 5 & precip < 10){f_iw <- 0.75}
      if (precip >= 10){f_iw <- 0.67}
      else {f_iw <- 'Error : [precip] must be positive. Please check that.'}
    }
    else if (design_speed == 100){
      if (precip == 0){f_iw <- 1.00}
      else if (precip > 0 & precip < 5){f_iw <- 0.87}
      else if (precip >= 5 & precip < 10){f_iw <- 0.75}
      else if (precip >= 10){f_iw <- 0.67}
      else {f_iw <- 'Error : [precip] must be positive. Please check that.'}
    }
    else if (design_speed == 120){
      if (precip == 0){f_iw <- 1.00}
      else if (precip > 0 & precip < 5){f_iw <- 0.87}
      else if (precip >= 5 & precip < 10){f_iw <- 0.75}
      else if (precip >= 10){f_iw <- 0.67}
      else {f_iw <- 'Error : [precip] must be positive. Please check that.'}
    }
    else {f_iw <- 'Error : [design_speed] must be one of [80], [100], [120]. Please check that.'}
  }
  else {f_iw <- 'Error : [weather] must be one of [sunny], [rainy], [snowy]. Please check that.'}
  f_iw
}
