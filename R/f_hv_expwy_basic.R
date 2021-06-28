#' Heavy Vehicle Factors in Basic Section of Expressway
#'
#' This function calculates Heavy Vehicle Factors by geographical features in basic section of expressway.
#' @param landform Choose one from : \code{'flatland'}, \code{'hill'}, \code{'mountain'}, \code{'specific_slope}
#' @param P_T1 The ratio of small heavy vehicle composition(Trucks less than 2.5 tons, buses less than 16 passengers)
#' @param P_T2 The ratio of middle heavy vehicle composition(Trucks of 2.5 tons or more, buses with 16 passengers or more)
#' @param P_T3 The ratio of large heavy vehicle composition(Semi trailer or full trailer)
#' @param P_hv The ratio of total heavy vehicles.
#' @param slope Slope gradient(%)
#' @param slope_length The length of the slope(km)
#' @export f_hv_expwy_basic Heavy Vehicle Factors
#' @details
#'     Argument 'landform' means :
#'     - \code{'flatland'} : Terrain in which heavy vehicles can travel at almost the same speed as passenger cars without being affected by terrain conditions in a combination of slope, flat alignment, and vertical alignment. Short slopes, typically less than 2%, are included.
#'     - \code{'hill'} : A place where the speed of heavy vehicles is lower than that of passenger cars, but not driving at the uphill limit speed for a fairly long period of time. Generally, slopes of 2% or more but less than 5% are included.
#'     - \code{'mountain'} : A place where heavy vehicles drive a fairly long section at the uphill limit speed or frequently drive at the uphill limit speed. Generally, slopes of 5% or more are included.
#'     - \code{'specific_slope'} : It is a single slope section with a slope of 3% or more and a slope length of 500m or more.
#' @seealso \code{\link{E_hv_expwy_basic}}
#' @examples
#' f_hv_expwy_basic(landform = 'flatland', P_T1 = 0.12, P_T2 = 0.08, P_T3 = 0.11)
#' f_hv_expwy_basic('hill', 0.21, 0.09, 0.04)
#' f_hv_expwy_basic('mountain', 0.2, 0.1, 0.05)
#' f_hv_expwy_basic(landform = 'specific_slope', P_hv = 0.38, slope = 3, slope_length = 4)
#' f_hv_expwy_basic(landform = 'specific_slope', P_T1 = 0.2, P_T2 = 0.1, P_T3 = 0.3, slope = 4.4, slope_length = 10)
f_hv_expwy_basic <- function(landform = NULL, P_T1 = NULL, P_T2 = NULL, P_T3 = NULL, P_hv = NULL, slope = NULL, slope_length = NULL){
  if (landform == 'flatland'){
    if (P_T1 >= 0 & P_T1 <= 1 & P_T2 >= 0 & P_T2 <= 1 & P_T3 >= 0 & P_T3 <= 1){
      f <- 1/(1 + P_T1 * (1.0 - 1) + P_T2 * (1.5 - 1) + P_T3 * (2.0 - 1))
    }
    else {
      f <- 'Error : [P_T1], [P_T2], [P_T3] must be >= 0 and <= 1. And [P_T1] + [P_T2] + [P_T3] must be <= 1. Please check that.'
      }
  }
  else if (landform == 'hill'){
    if (P_T1 >= 0 & P_T2 >= 0 & P_T3 >= 0 & (P_T1 + P_T2 + P_T3) <= 1){
      f <- 1/(1 + P_T1 * (1.2 - 1) + P_T2 * (3.0 - 1) + P_T3 * (3.0 - 1))
    }
    else {
      f <- 'Error : [P_T1], [P_T2], [P_T3] must be >= 0 and <= 1. And [P_T1] + [P_T2] + [P_T3] must be <= 1. Please check that.'
      }
  }
  else if (landform == 'mountain'){
    if (P_T1 >= 0 & P_T2 >= 0 & P_T3 >= 0 & (P_T1 + P_T2 + P_T3) <= 1){
      f <- 1/(1 + P_T1 * (1.5 - 1) + P_T2 * (5.0 - 1) + P_T3 * (5.0 - 1))
    }
    else {
      f <- 'Error : [P_T1], [P_T2], [P_T3] must be >= 0 and <= 1. And [P_T1] + [P_T2] + [P_T3] must be <= 1. Please check that.'
    }
  }
  else if (landform == 'specific_slope'){
    if (is.null(P_hv) == FALSE){
      if (P_hv >= 0 & P_hv <= 1){p <- P_hv}
      else {
        p <- NULL
        f <- 'Error : [P_hv] must be >= 0 and <= 1. Please check that.'}
    }
    else {
      if (P_T1 >= 0 & P_T2 >= 0 & P_T3 >= 0 & (P_T1 + P_T2 + P_T3) <= 1){
        p <- (P_T1 + P_T2 + P_T3)
        }
      else {
        p <- NULL
        f <- 'Error : [P_T1] + [P_T2] + [P_T3] must be >= 0 and <= 1. And each one must be >= 0. Please check that.'
        }
    }
    if (is.null(p) == FALSE & slope >= 0 & slope_length >= 0){
      ehv <- E_hv_expwy_basic(P_hv = p * 100, slope = slope, slope_length = slope_length)
      if (is.numeric(ehv) == TRUE){f <- 1/(1 + p * (ehv - 1))}
      else {f <- 'Error : Please check [P_hv], [slope] and [slope_length] values.'}
    }
    else {f <- 'Error : [P_hv] must be >= 0 and <= 1. Please check that.'}
  }
  else {f <- 'Error : [landform] must be one of them : [flatland], [hill], [mountain], [specific_slope]. Please check that.'}
  f
}
