#' Heavy Vehicle Factors(f_hv)
#'
#' This function calculates Heavy Vehicle Factors(f_hv) by geographical features in basic freeway section.
#' @param topography *String* Choose one from : \code{'flatland'}, \code{'hill'}, \code{'mountain'}, \code{'specific_slope}
#' @param prop_small_hv *Numeric* The ratio of small heavy vehicle composition(Trucks less than 2.5 tons, buses less than 16 passengers)
#' @param prop_middle_hv *Numeric* The ratio of middle heavy vehicle composition(Trucks of 2.5 tons or more, buses with 16 passengers or more)
#' @param prop_large_hv *Numeric* The ratio of large heavy vehicle composition(Semi trailer or full trailer)
#' @param prop_hv *Numeric* The ratio of heavy vehicles.
#' @param slope *Numeric* Slope rate(%)
#' @param slope_length *Numeric* The length of the slope(km)
#' @export f_hv Heavy Vehicle Factors(f_hv)
#' @examples
#' f_hv(topography = 'flatland', prop_small_hv = 0.3, prop_middle_hv = 0.1, prop_large_hv = 0.1
#' f_hv(topography = 'hill', prop_small_hv = 0.11, prop_middle_hv = 0.05, prop_large_hv = 0.03)
#' f_hv('mountain', 0.18, 0.14, 0)
#' f_hv('specific_slope', slope = 3.5, slope_length = 2, prop_hv = 0.3)
f_hv <- function(topography = NULL, prop_small_hv = 0, prop_middle_hv = 0, prop_large_hv = 0, prop_hv = 0, slope = 0, slope_length = 0){
  if (topography == 'flatland'){
    factor = 1/(1 + prop_small_hv * (1.0 - 1) + prop_middle_hv * (1.5 - 1) + prop_large_hv * (2.0 - 1))
  }
  if (topography == 'hill'){
    factor = 1/(1 + prop_small_hv * (1.2 - 1) + prop_middle_hv * (3.0 - 1) + prop_large_hv * (3.0 - 1))
  }
  if (topography == 'mountain'){
    factor = 1/(1 + prop_small_hv * (1.5 - 1) + prop_middle_hv * (5.0 - 1) + prop_large_hv * (5.0 - 1))
  }
  if (topography == 'specific_slope'){
    if (slope >= 0 & slope < 2){
      if (is.null(prop_small_hv) == FALSE & is.null(prop_middle_hv) == FALSE & is.null(prop_large_hv) == FALSE){
        prop_hv <- prop_small_hv + prop_middle_hv + prop_large_hv
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 1.5}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 1.5}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 1.5}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 1.5}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 1.5}
        if (prop_hv >= 40){E_hv = 1.5}
      }
      if (is.null(prop_hv) == FALSE){E_hv = 1.5}
    }
    if (slope >= 2 & slope < 3){
      if (slope_length >= 0 & slope_length < 0.5){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 1.5}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 1.5}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 1.5}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 1.5}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 1.5}
        if (prop_hv >= 40){E_hv = 1.5}
      }
      if (slope_length >= 0.5 & slope_length < 1.0){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 1.5}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 1.5}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 1.5}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 1.5}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 1.5}
        if (prop_hv >= 40){E_hv = 1.5}
      }
      if (slope_length >= 1.0 & slope_length < 1.5){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 1.5}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 1.5}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 1.5}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 1.5}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 1.5}
        if (prop_hv >= 40){E_hv = 1.5}
      }
      if (slope_length >= 1.5 & slope_length < 1.8){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 2.0}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 2.0}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 2.0}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 1.5}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 1.5}
        if (prop_hv >= 40){E_hv = 1.5}
      }
      if (slope_length >= 1.8 & slope_length < 2.5){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 2.5}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 2.0}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 2.0}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 2.0}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 2.0}
        if (prop_hv >= 40){E_hv = 2.0}
      }
      if (slope_length >= 2.5){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 3.0}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 2.5}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 2.0}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 2.0}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 2.0}
        if (prop_hv >= 40){E_hv = 2.0}
      }
    }
    if (slope >= 3 & slope < 4){
      if (slope_length >= 0 & slope_length < 0.5){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 1.5}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 1.5}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 1.5}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 1.5}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 1.5}
        if (prop_hv >= 40){E_hv = 1.5}
      }
      if (slope_length >= 0.5 & slope_length < 1.0){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 1.5}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 1.5}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 1.5}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 1.5}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 1.5}
        if (prop_hv >= 40){E_hv = 1.5}
      }
      if (slope_length >= 1.0 & slope_length < 1.2){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 2.0}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 2.0}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 2.0}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 1.5}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 1.5}
        if (prop_hv >= 40){E_hv = 1.5}
      }
      if (slope_length >= 1.2 & slope_length < 1.5){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 3.0}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 2.5}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 2.0}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 2.0}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 2.0}
        if (prop_hv >= 40){E_hv = 2.0}
      }
      if (slope_length >= 1.5 & slope_length < 1.8){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 3.5}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 3.0}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 2.0}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 2.0}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 2.0}
        if (prop_hv >= 40){E_hv = 2.0}
      }
      if (slope_length >= 1.8){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 4.0}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 3.0}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 2.5}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 2.0}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 2.0}
        if (prop_hv >= 40){E_hv = 2.0}
      }
    }
    if (slope >= 4 & slope < 5){
      if (slope_length >= 0 & slope_length < 0.4){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 1.5}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 1.5}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 1.5}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 1.5}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 1.5}
        if (prop_hv >= 40){E_hv = 1.5}
      }
      if (slope_length >= 0.4 & slope_length < 0.5){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 1.5}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 1.5}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 1.5}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 1.5}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 1.5}
        if (prop_hv >= 40){E_hv = 1.5}
      }
      if (slope_length >= 0.5 & slope_length < 0.8){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 2.0}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 2.0}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 2.0}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 1.5}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 1.5}
        if (prop_hv >= 40){E_hv = 1.5}
      }
      if (slope_length >= 0.8 & slope_length < 1.0){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 4.0}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 3.0}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 2.5}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 2.0}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 2.0}
        if (prop_hv >= 40){E_hv = 2.0}
      }
      if (slope_length >= 1.0 & slope_length < 1.5){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 5.0}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 4.0}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 3.0}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 3.0}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 2.5}
        if (prop_hv >= 40){E_hv = 2.0}
      }
      if (slope_length >= 1.5){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 5.5}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 4.0}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 3.5}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 3.0}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 3.0}
        if (prop_hv >= 40){E_hv = 2.5}
      }
    }
    if (slope >= 5 & slope < 6){
      if (slope_length >= 0 & slope_length < 0.4){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 1.5}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 1.5}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 1.5}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 1.5}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 1.5}
        if (prop_hv >= 40){E_hv = 1.5}
      }
      if (slope_length >= 0.4 & slope_length < 0.5){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 2.0}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 2.0}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 2.0}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 2.0}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 1.5}
        if (prop_hv >= 40){E_hv = 1.5}
      }
      if (slope_length >= 0.5 & slope_length < 0.8){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 4.0}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 3.0}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 2.5}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 2.0}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 2.0}
        if (prop_hv >= 40){E_hv = 2.0}
      }
      if (slope_length >= 0.8 & slope_length < 1.0){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 6.0}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 4.5}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 4.0}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 3.0}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 3.0}
        if (prop_hv >= 40){E_hv = 2.5}
      }
      if (slope_length >= 1.0 & slope_length < 1.5){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 6.5}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 5.0}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 4.0}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 4.0}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 3.0}
        if (prop_hv >= 40){E_hv = 3.0}
      }
      if (slope_length >= 1.5){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 7.0}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 5.0}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 4.5}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 4.0}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 3.5}
        if (prop_hv >= 40){E_hv = 3.0}
      }
    }
    if (slope >= 6 & slope < 7){
      if (slope_length >= 0 & slope_length < 0.4){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 2.0}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 2.0}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 1.5}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 1.5}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 1.5}
        if (prop_hv >= 40){E_hv = 1.5}
      }
      if (slope_length >= 0.4 & slope_length < 0.5){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 4.0}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 3.0}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 2.5}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 2.0}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 2.0}
        if (prop_hv >= 40){E_hv = 2.0}
      }
      if (slope_length >= 0.5 & slope_length < 0.8){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 6.0}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 4.5}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 4.0}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 3.0}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 2.5}
        if (prop_hv >= 40){E_hv = 2.5}
      }
      if (slope_length >= 0.8 & slope_length < 1.0){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 7.5}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 6.0}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 5.0}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 4.5}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 4.0}
        if (prop_hv >= 40){E_hv = 3.5}
      }
      if (slope_length >= 1.0 & slope_length < 1.5){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 8.0}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 6.0}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 5.5}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 5.0}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 4.0}
        if (prop_hv >= 40){E_hv = 3.5}
      }
      if (slope_length >= 1.5){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 8.0}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 6.5}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 5.5}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 5.0}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 4.0}
        if (prop_hv >= 40){E_hv = 3.5}
      }
    }
    if (slope >= 7 & slope < 8){
      if (slope_length >= 0 & slope_length < 0.4){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 3.0}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 2.5}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 2.0}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 2.0}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 2.0}
        if (prop_hv >= 40){E_hv = 2.0}
      }
      if (slope_length >= 0.4 & slope_length < 0.5){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 6.0}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 5.0}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 4.0}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 3.0}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 2.5}
        if (prop_hv >= 40){E_hv = 2.0}
      }
      if (slope_length >= 0.5 & slope_length < 0.8){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 8.0}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 6.0}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 5.0}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 4.5}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 4.0}
        if (prop_hv >= 40){E_hv = 3.5}
      }
      if (slope_length >= 0.8 & slope_length < 1.0){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 9.0}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 7.5}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 6.5}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 6.0}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 5.0}
        if (prop_hv >= 40){E_hv = 4.0}
      }
      if (slope_length >= 1.0 & slope_length < 1.5){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 9.5}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 7.5}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 7.0}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 6.0}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 5.0}
        if (prop_hv >= 40){E_hv = 4.0}
      }
      if (slope_length >= 1.5){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 9.5}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 7.5}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 7.0}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 6.0}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 5.0}
        if (prop_hv >= 40){E_hv = 4.0}
      }
    }
    if (slope >= 8){
      if (slope_length >= 0 & slope_length < 0.4){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 5.0}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 3.5}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 3.0}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 2.0}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 2.0}
        if (prop_hv >= 40){E_hv = 2.0}
      }
      if (slope_length >= 0.4 & slope_length < 0.5){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 8.0}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 6.0}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 5.5}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 4.0}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 4.0}
        if (prop_hv >= 40){E_hv = 3.5}
      }
      if (slope_length >= 0.5 & slope_length < 0.8){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 10.0}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 8.0}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 7.0}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 6.5}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 5.5}
        if (prop_hv >= 40){E_hv = 4.5}
      }
      if (slope_length >= 0.8 & slope_length < 1.0){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 10.5}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 9.0}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 8.0}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 7.0}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 5.5}
        if (prop_hv >= 40){E_hv = 4.5}
      }
      if (slope_length >= 1.0 & slope_length < 1.5){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 11.0}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 9.0}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 8.0}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 7.0}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 5.5}
        if (prop_hv >= 40){E_hv = 4.5}
      }
      if (slope_length >= 1.5){
        if (prop_hv >= 0 & prop_hv < 5){E_hv = 11.0}
        if (prop_hv >= 5 & prop_hv < 10){E_hv = 9.0}
        if (prop_hv >= 10 & prop_hv < 20){E_hv = 8.0}
        if (prop_hv >= 20 & prop_hv < 30){E_hv = 7.0}
        if (prop_hv >= 30 & prop_hv < 40){E_hv = 5.5}
        if (prop_hv >= 40){E_hv = 4.5}
      }
    }
    factor = 1/(1 + prop_hv * (E_hv - 1))
  }
  factor
}
