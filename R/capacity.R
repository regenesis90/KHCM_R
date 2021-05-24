#' Capacity(c, vph)
#'
#' This function calculates the capacity(vph) for a given road and traffic condition.
#' @param design_speed *Categorical*
#' @param N *Numeric* The number of freeway lane(one-way lane). It must be 2 or more.
#' @param f_w *Numeric* The One Side Lane Width and Lateral Clearance Factor(f_w). It could be calculated by f_w()
#' @param f_hv *Numeric* Heavy Vehicle Factors(f_hv). See f_hv()
#' @param weather *Categorical* Choose one from : \code{'sunny'}, \code{'rainy'}, \code{'snowy'}
#' @param day_night *Categorical* Choose one from : \code{'day'}, \code{'night'}
#' @export capacity capacity(vph)
#' @examples
#' capacity(design_speed = 100, c_j = 1500, N = 4, f_w = 0.79, f_hv = 0.62)
#' capacity(design_speed = 120, c_j = 1000, N = 3, f_w = 0.8, f_hv = 0.5, weather = 'sunny', day_night = 'day')
#' capacity(design_speed = 100, c_j = 900, N = 4, f_w = 0.82, f_hv = 0.63, weather = 'rainy', precipitation = 8.2, day_night = 'night')
capacity <- function(design_speed = NULL, N = NULL, f_w = NULL, f_hv = NULL, weather = 'sunny', precipitation = NULL, day_night = 'day'){
  if (N >=2 & f_w >= 0 & f_hv >=0){
    factor_weather <- f_iw(design_speed = design_speed, weather = weather, precipitation = precipitation)
    factor_day <- f_dk(design_speed = design_speed, day_night = day_night)
    if (design_speed == 120){c_j <- 2300}
    if (design_speed == 100){c_j <- 2200}
    if (design_speed == 80){c_j <- 2000}
    c_j * N * f_w * f_hv * factor_weather * factor_day
  }
}
