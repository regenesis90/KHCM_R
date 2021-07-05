#' Level of Service for Cyclist on Bicycle-only Road
#'
#' It derives Level of Service(LOS) for cyclist on bicycle-only road(type1)
#'     * The traffic volume by LOS is the traffic volume corresponding to the number of conflicts obtained by assuming the average value of bicycle speed of 13.5 kph and standard deviation of 3 kph.
#' It follows <Table 15-2>, <Table 15-5> in KHCM(2013), p.653-654.
#' @param road \code{'one-way'}, \code{'bidirectional'}
#' @param width \code{2}, \code{3}
#' @param confl Number of conflict
#' @param Q Bicycle Traffic (vph)
#' @param prop Proportion of bicycle traffic in the direction of travel in total (bidirectional) bicycle traffic when \code{direction = 'bidirectional'}. Choose one from : \code{0.3}, \code{0.4}, \code{0.5}, \code{0.6}, \code{0.7}
#' @keywords Level of Service LOS cyclist bicycle road bicycle-only
#' @export LOS_type1_bk
#' @examples
#' LOS_type1_bk(road = 'one-way', width = 2, Q = 642)
#' LOS_type1_bk(road = 'bidirectional', width = 3, confl = 324)
#' LOS_type1_bk(road = 'bidirectional', width = 2, prop = 0.5, Q = 102)
LOS_type1_bk <- function(road = NULL, width = NULL, confl = NULL, Q = NULL, prop = NULL){
  if (road == 'one-way'){
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
  }
  else if (road == 'bidirectional'){
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
        if (prop == 0.3){
          if (Q >= 0 & Q <= 52){los <- 'A'}
          else if (Q > 52 & Q <= 77){los <- 'B'}
          else if (Q > 77 & Q <= 129){los <- 'C'}
          else if (Q > 129 & Q <= 194){los <- 'D'}
          else if (Q > 194 & Q <= 252){los <- 'E'}
          else if (Q > 252){los <- 'F'}
          else {los <- 'Error : [Q] must be positive(vph). Please check that.'}
        }
        else if (prop == 0.4){
          if (Q >= 0 & Q <= 57){los <- 'A'}
          else if (Q > 57 & Q <= 86){los <- 'B'}
          else if (Q > 86 & Q <= 143){los <- 'C'}
          else if (Q > 143 & Q <= 214){los <- 'D'}
          else if (Q > 214 & Q <= 279){los <- 'E'}
          else if (Q > 279){los <- 'F'}
          else {los <- 'Error : [Q] must be positive(vph). Please check that.'}
        }
        else if (prop == 0.5){
          if (Q >= 0 & Q <= 64){los <- 'A'}
          else if (Q > 64 & Q <= 96){los <- 'B'}
          else if (Q > 96 & Q <= 160){los <- 'C'}
          else if (Q > 160 & Q <= 240){los <- 'D'}
          else if (Q > 240 & Q <= 312){los <- 'E'}
          else if (Q > 312){los <- 'F'}
          else {los <- 'Error : [Q] must be positive(vph). Please check that.'}
        }
        else if (prop == 0.6){
          if (Q >= 0 & Q <= 73){los <- 'A'}
          else if (Q > 73 & Q <= 109){los <- 'B'}
          else if (Q > 109 & Q <= 182){los <- 'C'}
          else if (Q > 182 & Q <= 273){los <- 'D'}
          else if (Q > 273 & Q <= 355){los <- 'E'}
          else if (Q > 355){los <- 'F'}
          else {los <- 'Error : [Q] must be positive(vph). Please check that.'}
        }
        else if (prop == 0.7){
          if (Q >= 0 & Q <= 84){los <- 'A'}
          else if (Q > 84 & Q <= 126){los <- 'B'}
          else if (Q > 126 & Q <= 211){los <- 'C'}
          else if (Q > 211 & Q <= 316){los <- 'D'}
          else if (Q > 316 & Q <= 411){los <- 'E'}
          else if (Q > 411){los <- 'F'}
          else {los <- 'Error : [Q] must be positive(vph). Please check that.'}
        }
        else {los <- 'Error : [prop] must be one of 0.3, 0.4, 0.5, 0.6, 0.7. Please check that.'}
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
        if (prop == 0.3){
          if (Q >= 0 & Q <= 116){los <- 'A'}
          else if (Q > 116 & Q <= 185){los <- 'B'}
          else if (Q > 185 & Q <= 271){los <- 'C'}
          else if (Q > 271 & Q <= 387){los <- 'D'}
          else if (Q > 387 & Q <= 484){los <- 'E'}
          else if (Q > 484){los <- 'F'}
          else {los <- 'Error : [Q] must be positive(vph). Please check that.'}
        }
        else if (prop == 0.4){
          if (Q >= 0 & Q <= 129){los <- 'A'}
          else if (Q > 129 & Q <= 200){los <- 'B'}
          else if (Q > 200 & Q <= 300){los <- 'C'}
          else if (Q > 300 & Q <= 429){los <- 'D'}
          else if (Q > 429 & Q <= 536){los <- 'E'}
          else if (Q > 536){los <- 'F'}
          else {los <- 'Error : [Q] must be positive(vph). Please check that.'}
        }
        else if (prop == 0.5){
          if (Q >= 0 & Q <= 144){los <- 'A'}
          else if (Q > 144 & Q <= 224){los <- 'B'}
          else if (Q > 224 & Q <= 336){los <- 'C'}
          else if (Q > 336 & Q <= 480){los <- 'D'}
          else if (Q > 480 & Q <= 620){los <- 'E'}
          else if (Q > 620){los <- 'F'}
          else {los <- 'Error : [Q] must be positive(vph). Please check that.'}
        }
        else if (prop == 0.6){
          if (Q >= 0 & Q <= 164){los <- 'A'}
          else if (Q > 164 & Q <= 255){los <- 'B'}
          else if (Q > 255 & Q <= 382){los <- 'C'}
          else if (Q > 382 & Q <= 545){los <- 'D'}
          else if (Q > 545 & Q <= 682){los <- 'E'}
          else if (Q > 682){los <- 'F'}
          else {los <- 'Error : [Q] must be positive(vph). Please check that.'}
        }
        else if (prop == 0.7){
          if (Q >= 0 & Q <= 190){los <- 'A'}
          else if (Q > 190 & Q <= 280){los <- 'B'}
          else if (Q > 280 & Q <= 442){los <- 'C'}
          else if (Q > 442 & Q <= 632){los <- 'D'}
          else if (Q > 632 & Q <= 790){los <- 'E'}
          else if (Q > 790){los <- 'F'}
          else {los <- 'Error : [Q] must be positive(vph). Please check that.'}
        }
        else {los <- 'Error : [prop] must be one of 0.3, 0.4, 0.5, 0.6, 0.7. Please check that.'}
      }
    }
    else {los <- 'Error : [width] must be one of 2, 3. Please check that.'}
  }
  else {los <- 'Error : [road] must be one of [one-way] or [bidirectional]. Please check that.'}
  los
}
