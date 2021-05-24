#' Capacity Calculation Correction Factor for Bad Weather Conditions in The Basic Section of The Freeway(f_iw)
#'
#' This function decides the capacity calculation factor for the basic section of the freeway(f_dk).
#' @param design_speed *Categorical* Choos one from : \code{120}, \code{100}, \code{80}
#' @param weather *Categorical* Choose one from : \code{'sunny'}, \code{'rainy'}, \code{'snowy'}
#' @param precipitation *Numeric* Rainfall or snowfall or hail (mm/h)
#' @export f_iw Capacity Calculation Correction Factor for Bad Weather Conditions in The Basic Section of The Freeway
#' @examples
#' f_iw(design_speed = 80, weather = 'sunny')
#' f_iw(design_speed = 100, weather = 'rainy', precipitation = 8)
#' f_iw(120, 'snowy', 12)
f_iw <- function(design_speed = NULL, weather = NULL, precipitation = NULL){
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
