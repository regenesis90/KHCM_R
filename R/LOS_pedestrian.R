#' Level of Service(LOS) for Pedestrian in Pedestrian Road
#'
#' It follows <Table 14-1> in KHCM(2013), p.616
#' @param V Pedestrian traffic flow rate (person/min/m). See V_pedestrian()
#' @param S Walking speed (m/min)
#' @param D Walking density (person/㎡)
#' @param M Pedestrian occupied space (㎡/person)
#' @export LOS_pedestrian
#' @examples
#' LOS_pedestrian(V = 32.1)
#' LOS_pedestrian(S = 15.32, D = 2.3)
#' LOS_pedestrian(S = 43.21, M = 19.22)
LOS_pedestrian <- function(V = NULL, S = NULL, D = NULL, M = NULL){
  if (is.null(V) == FALSE){
    if (V >= 0 & V <= 20){los <- 'A'}
    if (V > 20 & V <= 32){los <- 'B'}
    if (V > 32 & V <= 46){los <- 'C'}
    if (V > 46 & V <= 70){los <- 'D'}
    if (V > 70 & V <= 106){los <- 'E'}
    if (V > 106){los <- 'F'}
  }
  else{
    if (is.null(D) == FALSE){
      V <- V_pedestrian(S = S, D = D)
      if (V >= 0 & V <= 20){los <- 'A'}
      if (V > 20 & V <= 32){los <- 'B'}
      if (V > 32 & V <= 46){los <- 'C'}
      if (V > 46 & V <= 70){los <- 'D'}
      if (V > 70 & V <= 106){los <- 'E'}
      if (V > 106){los <- 'F'}
    }
    else{
      if (is.null(M) == FALSE){
        V <- V_pedestrian(S = S, M = M)
        if (V >= 0 & V <= 20){los <- 'A'}
        if (V > 20 & V <= 32){los <- 'B'}
        if (V > 32 & V <= 46){los <- 'C'}
        if (V > 46 & V <= 70){los <- 'D'}
        if (V > 70 & V <= 106){los <- 'E'}
        if (V > 106){los <- 'F'}
      }
      else{'At least one of D or M is needed.'}
    }
  }
  los
}
