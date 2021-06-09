#' Level of Service(LOS) for Pedestrian in Stairs
#'
#' It follows <Table 14-1> in KHCM(2013), p.616
#' @param group Choose one from: \code{'yes'}, \code{'no'}
#' @param V Pedestrian traffic flow rate (person/min/m). See V_pedestrian()
#' @param S Walking speed (m/min)
#' @param D Walking density (person/㎡)
#' @param M Pedestrian occupied space (㎡/person)
#' @export LOS_pedestrian_stair
#' @examples
#' LOS_pedestrian_stair(group = 'yes', V = 31)
#' LOS_pedestrian_stair(group = 'no', S = 13.2, D = 33.92)
#' LOS_pedestrian_stair(group = 'yes', S = 75.233, M = 21.74)
LOS_pedestrian_stair <- function(group = NULL, V = NULL, S = NULL, D = NULL, M = NULL){
  if (is.null(V) == FALSE){v <- V}
  else{
    if (is.null(D) == FALSE){v <- V_pedestrian(S = S, D = D)}
    else{
      if (is.null(M) == FALSE){v <- V_pedestrian(S = S, M = M)}
      else{'At least one of D or M is needed.'}
    }
  }
  if (group == 'no'){
    if (v >= 0 & v <= 18){los <- 'A'}
    if (v > 18 & v <= 20){los <- 'B'}
    if (v > 20 & v <= 25){los <- 'C'}
    if (v > 25 & v <= 32){los <- 'D'}
    if (v > 32 & v <= 52){los <- 'E'}
    if (v > 52){los <- 'F'}
  }
  if (group == 'yes'){
    if (v >= 0 & v <= 43){los <- 'A'}
    if (v > 43 & v <= 50){los <- 'B'}
    if (v > 50 & v <= 65){los <- 'C'}
    if (v > 95 & v <= 69){los <- 'D'}
    if (v > 69 & v <= 74){los <- 'E'}
    if (v > 74){los <- 'F'}
  }
  los
}
