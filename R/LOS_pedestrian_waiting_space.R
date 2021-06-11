#' Level of Service(LOS) for Pedestrian in Waiting Space
#'
#' It follows <Table 14-5> in KHCM(2013), p.618
#' @param D Walking density (person/㎡)
#' @param M Pedestrian occupied space (㎡/person)
#' @export LOS_pedestrian_waiting_space
#' @examples
#' LOS_pedestrian_waiting_space(D = 0.45)
#' LOS_pedestrian_waiting_space(M = 1.44456)
LOS_pedestrian_waiting_space <- function(D = NULL, M = NULL){
  if (is.null(D) == FALSE){
    if (D >= 1.0){los <- 'A'}
    if (D >= 0.8 & D < 1.0){los <- 'B'}
    if (D >= 0.6 & D < 0.8){los <- 'C'}
    if (D >= 0.4 & D < 0.6){los <- 'D'}
    if (D >= 0.2 & D < 0.4){los <- 'E'}
    if (D < 0.2){los <- 'F'}
  }
  else{
    if (is.null(M) == FALSE){
      if (M <= 1.1){los <- 'A'}
      if (M <= 1.6 & M > 1.1){los <- 'B'}
      if (M <= 2.0 & M > 1.6){los <- 'C'}
      if (M <= 2.5 & M > 2.0){los <- 'D'}
      if (M <= 5.0 & M > 2.5){los <- 'E'}
      if (M > 5.0){los <- 'F'}
    }
  }
  los
}
