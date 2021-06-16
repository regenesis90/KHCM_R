#' Correction Factor for Capacity Estimation in Case of Bad Weather in Basic Section of Expressway
#'
#' It is a coefficient indicating the effect of bad weather on traffic flow under basic road traffic conditions.
#'     It follows <Table 2-11> in KHCM(2013), p.41.
#' @param design_speed Design speed(kph). Choose one from : \code{120}, \code{100}, \code{80}
#' @param weather Weather condition. Choose one from : \code{'sunny'}, \code{'rainy'}, \code{'snowy'}
#' @param precipitation Rainfall or snowfall or hail (mm/h)
#' @export f_iw
#' @examples
#' f_iw_expwy_basic(design_speed = 80, weather = 'sunny')
#' f_iw_expwy_basic(design_speed = 100, weather = 'rainy', precipitation = 8)
#' f_iw_expwy_basic(120, 'snowy', 12)
f_iw_expwy_basic <- function(design_speed = NULL, weather = NULL, precipitation = NULL){
  if (weather == "sunny"){f_iw <- 1.00}
  if (weather == "rainy"){
    if (design_speed == 80){
      if (precipitation == 0){f_iw <- 1.00}
      if (precipitation > 0 & precipitation < 5){f_iw <- 0.97}
      if (precipitation >= 5 & precipitation < 10){f_iw <- 0.92}
      if (precipitation >= 10){f_iw <- 0.90}
    }
    if (design_speed == 100){
      if (precipitation == 0){f_iw <- 1.00}
      if (precipitation > 0 & precipitation < 5){f_iw <- 0.94}
      if (precipitation >= 5 & precipitation < 10){f_iw <- 0.90}
      if (precipitation >= 10){f_iw <- 0.86}
    }
    if (design_speed == 120){
      if (precipitation == 0){f_iw <- 1.00}
      if (precipitation > 0 & precipitation < 5){f_iw <- 0.92}
      if (precipitation >= 5 & precipitation < 10){f_iw <- 0.86}
      if (precipitation >= 10){f_iw <- 0.81}
    }
  }
  if (weather == "snowy"){
    if (design_speed == 80){
      if (precipitation == 0){f_iw <- 1.00}
      if (precipitation > 0 & precipitation < 5){f_iw <- 0.87}
      if (precipitation >= 5 & precipitation < 10){f_iw <- 0.75}
      if (precipitation >= 10){f_iw <- 0.67}
    }
    if (design_speed == 100){
      if (precipitation == 0){f_iw <- 1.00}
      if (precipitation > 0 & precipitation < 5){f_iw <- 0.87}
      if (precipitation >= 5 & precipitation < 10){f_iw <- 0.75}
      if (precipitation >= 10){f_iw <- 0.67}
    }
    if (design_speed == 120){
      if (precipitation == 0){f_iw <- 1.00}
      if (precipitation > 0 & precipitation < 5){f_iw <- 0.87}
      if (precipitation >= 5 & precipitation < 10){f_iw <- 0.75}
      if (precipitation >= 10){f_iw <- 0.67}
    }
  }
  f_iw
}
