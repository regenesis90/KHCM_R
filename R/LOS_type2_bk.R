#' Level of Service for Cyclist on Bicycle-Pedestrian Road
#'
#' It derives Level of Service(LOS) for cyclist on bicycle-pedestrain road(type2).
#'     * The traffic volume by LOS is the traffic volume corresponding to the number of conflicts obtained by assuming the average value of bicycle speed of 13.5 kph and standard deviation of 3 kph.
#'     * It follows <Table 15-2> ~ <Table 15-5> in KHCM(2013), p.653~654.
#' @param width Lane width of city road(m). Choose one from : \code{2}, \code{3}
#' @param confl Number of conflict
#' @param Q Bidirectional Bicycle Traffic (vph)
#' @keywords Level of service LOS cyclist bicycle pat city road
#' @export LOS_type2_bk
#' @examples
#' LOS_type2_bk(width = 2, confl = 183)
#' LOS_type2_bk(width = 3, Q = 343)
LOS_type2_bk <- function(width = NULL, confl = NULL, Q = NULL){
  if (width == 2){
    if (is.null(confl) == FALSE){
      if (confl >= 0 & confl <= 40){los <- 'A'}
      else if (confl > 40 & confl <= 60){los <- 'B'}
      else if (confl > 60 & confl <= 100){los <- 'C'}
      else if (confl > 100 & confl <= 150){los <- 'D'}
      else if (confl > 150 & confl <= 195){los <- 'E'}
      else if (confl > 195){los <- 'F'}
      else {los <- 'Error : [confl] must be positive. Please check that.'}
    }
    else{
      if (Q >= 0 & Q <= 60){los <- 'A'}
      else if (Q > 60 & Q <= 90){los <- 'B'}
      else if (Q > 90 & Q <= 150){los <- 'C'}
      else if (Q > 150 & Q <= 225){los <- 'D'}
      else if (Q > 225 & Q <= 295){los <- 'E'}
      else if (Q > 295){los <- 'F'}
      else {los <- 'Error : [Q] must be positive(vph). Please check that.'}
    }
  }
  else if (width == 3){
    if (is.null(confl) == FALSE){
      if (confl >= 0 & confl <= 90){los <- 'A'}
      else if (confl > 90 & confl <= 140){los <- 'B'}
      else if (confl > 140 & confl <= 210){los <- 'C'}
      else if (confl > 210 & confl <= 300){los <- 'D'}
      else if (confl > 300 & confl <= 375){los <- 'E'}
      else if (confl > 375){los <- 'F'}
      else {los <- 'Error : [confl] must be positive. Please check that.'}
    }
    else{
      if (Q >= 0 & Q <= 135){los <- 'A'}
      else if (Q > 135 & Q <= 210){los <- 'B'}
      else if (Q > 210 & Q <= 315){los <- 'C'}
      else if (Q > 315 & Q <= 450){los <- 'D'}
      else if (Q > 450 & Q <= 565){los <- 'E'}
      else if (Q > 565){los <- 'F'}
      else {los <- 'Error : [Q] must be positive(vph). Please check that.'}
    }
  }
  else {los <- 'Error : [width] must be one of 2, 3. Please check that.'}
  los
}
