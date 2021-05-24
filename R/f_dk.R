#' Capacity Calculation Correction Factor for The Night Situation of The Basic Section of The Freeway(f_dk)
#'
#' This function decides the capacity calculation factor for the basic section of the freeway(f_dk).
#' @param design_speed *Categorical* Choos one from : \code{120}, \code{100}, \code{80}
#' @param day_night *Categorical* Choose one from : \code{'day'}, \code{'night'}
#' @export f_dk Capacity Calculation Correction Factor for The Night Situation of The Basic Section of The Freeway
#' @examples
#' f_dk(design_speed = 100, 'day')
#' f_dk(design_speed = 120, 'night')
#' f_dk(80, 'night')
f_dk <- function(design_speed = NULL, day_night = NULL){
  if (day_night == "day"){f_dk <- 1.00}
  if (day_night == "night"){
    if (design_speed == 80){f_dk <- 0.97}
    if (design_speed == 100){f_dk <- 0.93}
    if (design_speed == 120){f_dk <- 0.91}
    }
  f_dk
}
