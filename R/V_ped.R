#' Pedestrian Traffic Flow Rate in Pedestrian Facilities
#'
#' It calculates pedestrian traffic flow rate(person/min/m) in pedestrian facilities.
#'     It follows <Formula 14-1>, <Formula 14-2> in KHCM(2013), p.613.
#' @param S Walking speed(m/min).
#' @param D Walking density(person/㎡). It is same with \code{1/M}
#' @param M Pedestrian occupied space.(㎡/person) It is same with \code{1/D}
#' @export V_ped
#' @examples
#' V_ped(S = 50, D = 3)
#' V_ped(S = 32.3, M = 0.2)
V_ped <- function(S = NULL, D = NULL, M = NULL){
  if (S >= 0){
    if (is.null(D) == FALSE){
      if (D > 0){v <- S * D}
      else {v <- 'Error : [D] must be positive(person/㎡). Please check that.'}
    }
    else {
      if (is.null(M) == FALSE){
        if (M > 0){v <- S / M}
        else {v <- 'Error : [M] must be positive(㎡/person). Please check that.'}
      }
      else {v <- 'Error : One of [D] or [M] is necessary. Please check that.'}
    }
  }
  else {v <- 'Error : [S] must be positive(m/min). Please check that.'}
  v
}
