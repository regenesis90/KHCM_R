#' Level of Service for Cyclist on Basic Section of City Road, Bicycle Path
#'
#' It derives Level of Service(LOS) for cyclist on basic section of city road, bicycle path.
#'     * A bicycle path installed in proximity to the road traffic lane and separated by road markings on the road(type3)
#'     * The traffic volume by LOS is the traffic volume corresponding to the number of conflicts obtained by assuming the average value of bicycle speed of 13.5 kph and standard deviation of 3 kph.
#'     * It follows <Table 15-2> ~ <Table 15-5> in KHCM(2013), p.653~654.
#' @param width Lane width of city road(m). Choose one from : \code{2}, \code{3}
#' @param confl Number of conflict
#' @param Q Bicycle Traffic (vph)
#' @param p_b Proportion of bicycle traffic in the direction of travel in total (bidirectional) bicycle traffic
#' @keywords Level of service LOS cyclist bicycle pat city road
#' @export LOS_type3_bk
#' @examples
#' LOS_type3_bk(width = 2, confl = 183)
#' LOS_type3_bk(width = 3, Q = 343)
LOS_type3_bk <- function(width = NULL, confl = NULL, Q = NULL){
  if (width == 2){
    if (is.null(confl) == FALSE){
      if (confl >= 0 & confl <= 25){los <- 'A'}
      else if (confl > 25 & confl <= 50){los <- 'B'}
      else if (confl > 50 & confl <= 100){los <- 'C'}
      else if (confl > 100 & confl <= 180){los <- 'D'}
      else if (confl > 180 & confl <= 240){los <- 'E'}
      else if (confl > 240){los <- 'F'}
      else {los <- 'Error : [confl] must be positive. Please check that.'}
    }
    else{
      if (Q >= 0 & Q <= 100){los <- 'A'}
      else if (Q > 100 & Q <= 200){los <- 'B'}
      else if (Q > 200 & Q <= 400){los <- 'C'}
      else if (Q > 400 & Q <= 720){los <- 'D'}
      else if (Q > 720 & Q <= 960){los <- 'E'}
      else if (Q > 960){los <- 'F'}
      else {los <- 'Error : [Q] must be positive(vph). Please check that.'}
    }
  }
  else if (width == 3){
    if (is.null(confl) == FALSE){
      if (confl >= 0 & confl <= 150){los <- 'A'}
      else if (confl > 150 & confl <= 295){los <- 'B'}
      else if (confl > 295 & confl <= 590){los <- 'C'}
      else if (confl > 590 & confl <= 1030){los <- 'D'}
      else if (confl > 1030 & confl <= 1470){los <- 'E'}
      else if (confl > 1470){los <- 'F'}
      else {los <- 'Error : [confl] must be positive. Please check that.'}
    }
    else{
      if (Q >= 0 & Q <= 600){los <- 'A'}
      else if (Q > 600 & Q <= 1180){los <- 'B'}
      else if (Q > 1180 & Q <= 2360){los <- 'C'}
      else if (Q > 2360 & Q <= 4120){los <- 'D'}
      else if (Q > 4120 & Q <= 5880){los <- 'E'}
      else if (Q > 5880){los <- 'F'}
      else {los <- 'Error : [Q] must be positive(vph). Please check that.'}
    }
  }
  else {los <- 'Error : [width] must be one of 2, 3. Please check that.'}
  los
}
