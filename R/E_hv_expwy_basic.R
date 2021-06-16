#' Conversion factor for passenger cars on a specific slope section of a expressway
#'
#' This function calculates Conversion factor for passenger cars on a specific slope section of a expressway.
#'     It follows <Table 2-4> in KHCM(2013), p.27
#' @param P_hv *Numeric* The percentaget of heavy vehicles(%).
#' @param slope *Numeric* Slope rate(%).
#' @param slope_length *Numeric* The length of the slope(km)
#' @export E_hv Conversion factor for passenger cars on a specific slope section of a expressway.
#' @examples
#' E_hv_expwy_basic(P_hv = 32, slope = 3.1, slope_length = 2.8)
#' E_hv_expwy_basic(18.3, 5.9, 2.1)
E_hv_expwy_basic <- function(P_hv = 0, slope = 0, slope_length = 0){
  if (slope >= 0 & slope < 2){
    if (P_hv >= 0 & P_hv < 5){E_hv <- 1.5}
    else if (P_hv >= 5 & P_hv < 10){E_hv <- 1.5}
    else if (P_hv >= 10 & P_hv < 20){E_hv <- 1.5}
    else if (P_hv >= 20 & P_hv < 30){E_hv <- 1.5}
    else if (P_hv >= 30 & P_hv < 40){E_hv <- 1.5}
    else if (P_hv >= 40 & P_hv <= 100){E_hv <- 1.5}
    else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
  else if (slope >= 2 & slope < 3){
    if (slope_length >= 0 & slope_length < 0.5){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 1.5}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 1.5}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 1.5}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 1.5}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 1.5}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 1.5}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 0.5 & slope_length < 1.0){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 1.5}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 1.5}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 1.5}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 1.5}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 1.5}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 1.5}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 1.0 & slope_length < 1.5){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 1.5}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 1.5}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 1.5}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 1.5}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 1.5}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 1.5}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 1.5 & slope_length < 1.8){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 2.0}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 2.0}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 2.0}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 1.5}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 1.5}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 1.5}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 1.8 & slope_length < 2.5){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 2.5}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 2.0}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 2.0}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 2.0}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 2.0}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 2.0}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 2.5){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 3.0}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 2.5}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 2.0}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 2.0}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 2.0}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 2.0}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else {E_hv <- 'Error : [slope_length] must be positive value. Please check that.'}
  }
  else if (slope >= 3 & slope < 4){
    if (slope_length >= 0 & slope_length < 0.5){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 1.5}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 1.5}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 1.5}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 1.5}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 1.5}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 1.5}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 0.5 & slope_length < 1.0){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 1.5}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 1.5}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 1.5}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 1.5}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 1.5}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 1.5}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 1.0 & slope_length < 1.2){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 2.0}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 2.0}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 2.0}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 1.5}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 1.5}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 1.5}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 1.2 & slope_length < 1.5){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 3.0}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 2.5}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 2.0}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 2.0}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 2.0}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 2.0}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 1.5 & slope_length < 1.8){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 3.5}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 3.0}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 2.0}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 2.0}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 2.0}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 2.0}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 1.8){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 4.0}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 3.0}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 2.5}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 2.0}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 2.0}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 2.0}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else {E_hv <- 'Error : [slope_length] must be positive value. Please check that.'}
  }
  else if (slope >= 4 & slope < 5){
    if (slope_length >= 0 & slope_length < 0.4){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 1.5}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 1.5}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 1.5}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 1.5}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 1.5}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 1.5}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 0.4 & slope_length < 0.5){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 1.5}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 1.5}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 1.5}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 1.5}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 1.5}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 1.5}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 0.5 & slope_length < 0.8){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 2.0}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 2.0}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 2.0}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 1.5}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 1.5}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 1.5}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 0.8 & slope_length < 1.0){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 4.0}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 3.0}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 2.5}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 2.0}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 2.0}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 2.0}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 1.0 & slope_length < 1.5){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 5.0}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 4.0}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 3.0}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 3.0}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 2.5}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 2.0}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 1.5){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 5.5}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 4.0}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 3.5}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 3.0}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 3.0}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 2.5}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else {E_hv <- 'Error : [slope_length] must be positive value. Please check that.'}
  }
  else if (slope >= 5 & slope < 6){
    if (slope_length >= 0 & slope_length < 0.4){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 1.5}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 1.5}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 1.5}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 1.5}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 1.5}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 1.5}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 0.4 & slope_length < 0.5){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 2.0}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 2.0}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 2.0}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 2.0}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 1.5}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 1.5}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 0.5 & slope_length < 0.8){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 4.0}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 3.0}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 2.5}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 2.0}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 2.0}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 2.0}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 0.8 & slope_length < 1.0){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 6.0}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 4.5}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 4.0}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 3.0}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 3.0}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 2.5}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 1.0 & slope_length < 1.5){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 6.5}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 5.0}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 4.0}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 4.0}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 3.0}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 3.0}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 1.5){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 7.0}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 5.0}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 4.5}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 4.0}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 3.5}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 3.0}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else {E_hv <- 'Error : [slope_length] must be positive value. Please check that.'}
  }
  else if (slope >= 6 & slope < 7){
    if (slope_length >= 0 & slope_length < 0.4){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 2.0}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 2.0}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 1.5}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 1.5}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 1.5}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 1.5}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 0.4 & slope_length < 0.5){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 4.0}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 3.0}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 2.5}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 2.0}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 2.0}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 2.0}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 0.5 & slope_length < 0.8){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 6.0}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 4.5}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 4.0}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 3.0}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 2.5}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 2.5}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 0.8 & slope_length < 1.0){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 7.5}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 6.0}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 5.0}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 4.5}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 4.0}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 3.5}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 1.0 & slope_length < 1.5){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 8.0}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 6.0}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 5.5}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 5.0}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 4.0}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 3.5}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 1.5){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 8.0}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 6.5}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 5.5}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 5.0}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 4.0}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 3.5}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else {E_hv <- 'Error : [slope_length] must be positive value. Please check that.'}
  }
  else if (slope >= 7 & slope < 8){
    if (slope_length >= 0 & slope_length < 0.4){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 3.0}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 2.5}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 2.0}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 2.0}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 2.0}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 2.0}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 0.4 & slope_length < 0.5){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 6.0}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 5.0}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 4.0}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 3.0}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 2.5}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 2.0}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 0.5 & slope_length < 0.8){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 8.0}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 6.0}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 5.0}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 4.5}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 4.0}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 3.5}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 0.8 & slope_length < 1.0){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 9.0}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 7.5}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 6.5}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 6.0}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 5.0}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 4.0}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 1.0 & slope_length < 1.5){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 9.5}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 7.5}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 7.0}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 6.0}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 5.0}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 4.0}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 1.5){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 9.5}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 7.5}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 7.0}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 6.0}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 5.0}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 4.0}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else {E_hv <- 'Error : [slope_length] must be positive value. Please check that.'}
  }
  else if (slope >= 8){
    if (slope_length >= 0 & slope_length < 0.4){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 5.0}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 3.5}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 3.0}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 2.0}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 2.0}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 2.0}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 0.4 & slope_length < 0.5){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 8.0}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 6.0}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 5.5}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 4.0}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 4.0}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 3.5}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 0.5 & slope_length < 0.8){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 10.0}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 8.0}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 7.0}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 6.5}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 5.5}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 4.5}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 0.8 & slope_length < 1.0){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 10.5}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 9.0}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 8.0}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 7.0}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 5.5}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 4.5}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 1.0 & slope_length < 1.5){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 11.0}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 9.0}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 8.0}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 7.0}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 5.5}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 4.5}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else if (slope_length >= 1.5){
      if (P_hv >= 0 & P_hv < 5){E_hv <- 11.0}
      else if (P_hv >= 5 & P_hv < 10){E_hv <- 9.0}
      else if (P_hv >= 10 & P_hv < 20){E_hv <- 8.0}
      else if (P_hv >= 20 & P_hv < 30){E_hv <- 7.0}
      else if (P_hv >= 30 & P_hv < 40){E_hv <- 5.5}
      else if (P_hv >= 40 & P_hv <= 100){E_hv <- 4.5}
      else {E_hv <- 'Error : [P_hv] must be percentage value. Please check that.'}
    }
    else {E_hv <- 'Error : [slope_length] must be positive value. Please check that.'}
  }
  else {E_hv <- 'Error : [slope] must be positive value. Please check your slope.'}
  E_hv
}
