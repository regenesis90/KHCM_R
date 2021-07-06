#' Level of Service(LOS) of Pedestrian in Pedestrian Road
#'
#' Pedestrian service level on pedestrian roads.
#'     - If the value is out of service level E, it is determined as service level F.
#'     - Pedestrian safety, convenience, and comfort should be considered, rather than determined by comparing only the size of the pedestrian space.
#'     - It follows <Table 14-1> in KHCM(2013), p.616.
#' @param S Pedestrian walking speed(m/min).
#' @param D Pedestrian walking density(person/㎡).
#' @param M Pedestrian occupied space(㎡/person).
#' @keywords pedestrian LOS level of service road
#' @seealso \code{\link{V_ped}}
#' @export LOS_road_ped
#' @examples
#' LOS_road_ped(S = 15.32, D = 2.3)
#' LOS_road_ped(S = 43.21, M = 19.22)
LOS_road_ped <- function(S = NULL, D = NULL, M = NULL){
  V <- V_ped(S = S, D = D, M = M)
  if (is.numeric(V) == TRUE){
    if (V >= 0 & V <= 20){los <- 'A'}
    else if (V > 20 & V <= 32){los <- 'B'}
    else if (V > 32 & V <= 46){los <- 'C'}
    else if (V > 46 & V <= 70){los <- 'D'}
    else if (V > 70 & V <= 106){los <- 'E'}
    else if (V > 106){los <- 'F'}
    else {los <- 'Error : Please check [S], [D], [M].'}
  }
  else{
    if (is.null(M) == FALSE){
      if (M >= 3.3){los <- 'A'}
      else if (M >= 2.0 & M < 3.3){los <- 'B'}
      else if (M >= 1.4 & M < 2.0){los <- 'C'}
      else if (M >= 0.9 & M < 1.4){los <- 'D'}
      else if (M >= 0.38 & M < 0.9){los <- 'E'}
      else if (M < 0.38 & M > 0){los <- 'F'}
      else {los <- 'Error : [M] must be positive. Please check that.'}
    }
    else {
      if (is.null(D) == FALSE){
        if (D <= 0.3){los <- 'A'}
        else if (D <= 0.5 & D > 0.3){los <- 'B'}
        else if (D <= 0.7 & D > 0.5){los <- 'C'}
        else if (D <= 1.1 & D > 0.7){los <- 'D'}
        else if (D <= 2.6 & D > 1.1){los <- 'E'}
        else if (D > 2.6){los <- 'F'}
        else {los <- 'Error : [D] must be positive. Please check that.'}
      }
      else {
        if (is.null(S) == FALSE){
          if (S >= 75){los <- 'A'}
          else if (S >= 72 & S < 75){los <- 'B'}
          else if (S >= 69 & S < 72){los <- 'C'}
          else if (S >= 62 & S < 69){los <- 'D'}
          else if (S >= 40 & S < 62){los <- 'E'}
          else if (S < 40 & S > 0){los <- 'F'}
          else {los <- 'Error : [M] must be positive. Please check that.'}
        }
        else {los <- 'Error : [S] must be positive. Please check that.'}
      }
    }
  }
  los
}
