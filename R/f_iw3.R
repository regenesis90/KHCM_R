#' Capacity calculation correction factor for bad weather conditions at signal intersections(f_iw3)
#'
#' It follows <Table 8-28> in KHCM(2013)
#' @param weather *Categorical* Choose one from : \code{'sunny'}, \code{'rainy'}, \code{'snowy'}
#' @param precipitation *Numeric* Rainfall or snowfall or hail (mm/h)
#' @export f_iw3
#' @examples
#' f_iw3('sunny')
#' f_iw3('rainy', 32.1)
#' f_iw3('snowy', 8.2334)
f_iw3 <- function(weather = NULL, precipitation = NULL){
  if (weather == 'sunny'){f <- 1}
  if (weather == 'rainy'){
    if (precipitation == 0){f <- 1.00}
    if (precipitation > 0 & precipitation <= 2){f <- 0.93}
    if (precipitation > 2 & precipitation < 4.5){f <- 0.83}
    if (precipitation >= 4.5){f <- 0.79}
  }
  if (weather == 'snowy'){
    if (precipitation == 0){f <- 1.00}
    if (precipitation > 0 & precipitation <= 1){f <- 0.65}
    if (precipitation > 1 & precipitation <= 20){f <- 0.46}
    if (precipitation >= 21){f <- 0.34}
  }
  f
}
