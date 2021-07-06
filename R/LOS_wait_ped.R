#' Pedestrian Level of Service(LOS) in Waiting Area
#'
#' Pedestrian service level in the waiting area. According to the standard Korean body type,
#'     the area including the extra width is calculated as about 0.2㎡ (this is the standard for service level E),
#'     and this is expressed as the service level in the waiting space.
#'     It follows <Table 14-5> in KHCM(2013), p.618.
#' @param D Pedestrian walking density (person/㎡)
#' @param M Pedestrian occupied space (㎡/person)
#' @export LOS_wait_ped
#' @examples
#' LOS_wait_ped(D = 0.45)
#' LOS_wait_ped(M = 1.44456)
LOS_wait_ped <- function(D = NULL, M = NULL){
  if (is.null(D) == FALSE){
    if (D >= 1.0){los <- 'A'}
    else if (D >= 0.8 & D < 1.0){los <- 'B'}
    else if (D >= 0.6 & D < 0.8){los <- 'C'}
    else if (D >= 0.4 & D < 0.6){los <- 'D'}
    else if (D >= 0.2 & D < 0.4){los <- 'E'}
    else if (D < 0.2 & D > 0){los <- 'F'}
    else {los <- 'Error : [D] must be positive. Please check that.'}
  }
  else{
    if (is.null(M) == FALSE){
      if (M <= 1.1){los <- 'A'}
      else if (M <= 1.6 & M > 1.1){los <- 'B'}
      else if (M <= 2.0 & M > 1.6){los <- 'C'}
      else if (M <= 2.5 & M > 2.0){los <- 'D'}
      else if (M <= 5.0 & M > 2.5){los <- 'E'}
      else if (M > 5.0){los <- 'F'}
      else {los <- 'Error : [M] must be positive. Please check that.'}
    }
    else {los <- 'Error : At least one of [D] or [M] is necessary. Please check that.'}
  }
  los
}
