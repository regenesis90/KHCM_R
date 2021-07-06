#' Pedestrian Level of Service(LOS) on Stairs
#'
#' Pedestrian service level on stairs.
#'     This is presented by dividing the case when the pedestrian forms the platoon and the case where it does not.
#'     When pedestrians pass by forming a platoon, it can be observed mainly from the stairs of terminals and subway transfer passages.
#'     * When the peak 15-minute observed pedestrian traffic volume is 450 people/15 min/m or more = that is, when the peak 15 min traffic volume is 30 or more,
#'       it is considered to form a pedestrian group.
#'     * It follows <Table 14-1> in KHCM(2013), p.616
#' @param V_15 Observed peak hour 15-minute pedestrian traffic volume(person/15min/m)
#' @param S Walking speed (m/min)
#' @param D Walking density (person/㎡)
#' @param M Pedestrian occupied space (㎡/person)
#' @export LOS_stair_ped
#' @examples
#' LOS_stair_ped(V_15 = 322, S = 13.2, D = 33.92)
#' LOS_stair_ped(V_15 = 944, S = 75.233, M = 21.74)
LOS_stair_ped <- function(V_15 = NULL, S = NULL, D = NULL, M = NULL){
  v <- V_ped(S = S, D = D, M = M)
  if (is.numeric(v) == TRUE){
    if (V_15 < 450 & V_15 > 0){
      if (v >= 0 & v <= 18){los <- 'A'}
      else if (v > 18 & v <= 20){los <- 'B'}
      else if (v > 20 & v <= 25){los <- 'C'}
      else if (v > 25 & v <= 32){los <- 'D'}
      else if (v > 32 & v <= 52){los <- 'E'}
      else if (v > 52){los <- 'F'}
      else {los <- 'Error : Please check [S], [D], [M].'}
    }
    else if (V_15 >= 450){
      if (v >= 0 & v <= 43){los <- 'A'}
      else if (v > 43 & v <= 50){los <- 'B'}
      else if (v > 50 & v <= 65){los <- 'C'}
      else if (v > 95 & v <= 69){los <- 'D'}
      else if (v > 69 & v <= 74){los <- 'E'}
      else if (v > 74){los <- 'F'}
      else {los <- 'Error : Please check [S], [D], [M].'}
    }
    else {los <- 'Error : [V_15] must be positive(person/15min/m). Please check that.'}
  }
  else {los <- v}
  los
}
