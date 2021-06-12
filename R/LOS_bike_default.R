#' Service level in the basic section of bicycle-only roads, bicycle-pedestrian roads, and road bike roads
#'
#' It follows <Table 15-2> ~ <Table 15-5> in KHCM(2013), p.653~654
#' @param type *Categorical* \code{'bike_only'}, \code{'bike_ped_comb'}, \code{'road_basic'}
#' @param way *Categorical* \code{'one_way'}, \code{'two_way'}
#' @param width *Categorical* \code{2}, \code{3}
#' @param n_conflict Number of conflict
#' @param Q Total, or Bidirectional Bicycle Traffic (vph)
#' @param p_b Proportion of bicycle traffic in the direction of travel in total (bidirectional) bicycle traffic
#' @keywords
#' @export LOS_bike_default
#' @examples
#' LOS_bike_default(type = 'bike_only', way = 'two_way', width = 3, n_conflict = 324)
#' LOS_bike_default(type = 'bike_ped_comb', width = 2, Q = 132)
LOS_bike_default <- function(type = NULL, way = NULL, width = NULL, n_conflict = NULL, Q = NULL, p_b = NULL){
  if (type == 'bike_only'){
    if (way == 'one_way'){
      if (width == 2){
        if (is.null(n_conflict) == FALSE){
          if (n_conflict >= 0 & n_conflict <= 25){los <- 'A'}
          if (n_conflict > 25 & n_conflict <= 50){los <- 'B'}
          if (n_conflict > 50 & n_conflict <= 100){los <- 'C'}
          if (n_conflict > 100 & n_conflict <= 180){los <- 'D'}
          if (n_conflict > 180 & n_conflict <= 240){los <- 'E'}
          if (n_conflict > 240){los <- 'F'}
        }
        else{
          if (Q >= 0 & Q <= 100){los <- 'A'}
          if (Q > 100 & Q <= 200){los <- 'B'}
          if (Q > 200 & Q <= 400){los <- 'C'}
          if (Q > 400 & Q <= 720){los <- 'D'}
          if (Q > 720 & Q <= 960){los <- 'E'}
          if (Q > 960){los <- 'F'}
        }
      }
      if (width == 3){
        if (is.null(n_conflict) == FALSE){
          if (n_conflict >= 0 & n_conflict <= 150){los <- 'A'}
          if (n_conflict > 150 & n_conflict <= 295){los <- 'B'}
          if (n_conflict > 295 & n_conflict <= 590){los <- 'C'}
          if (n_conflict > 590 & n_conflict <= 1030){los <- 'D'}
          if (n_conflict > 1030 & n_conflict <= 1470){los <- 'E'}
          if (n_conflict > 1470){los <- 'F'}
        }
        else{
          if (Q >= 0 & Q <= 600){los <- 'A'}
          if (Q > 600 & Q <= 1180){los <- 'B'}
          if (Q > 1180 & Q <= 2360){los <- 'C'}
          if (Q > 2360 & Q <= 4120){los <- 'D'}
          if (Q > 4120 & Q <= 5880){los <- 'E'}
          if (Q > 5880){los <- 'F'}
        }
      }
    }
    if (way == 'two_way'){
      if (width == 2){
        if (is.null(n_conflict) == FALSE){
          if (n_conflict >= 0 & n_conflict <= 40){los <- 'A'}
          if (n_conflict > 40 & n_conflict <= 60){los <- 'B'}
          if (n_conflict > 60 & n_conflict <= 100){los <- 'C'}
          if (n_conflict > 100 & n_conflict <= 150){los <- 'D'}
          if (n_conflict > 150 & n_conflict <= 195){los <- 'E'}
          if (n_conflict > 195){los <- 'F'}
        }
        else{
          if (p_b == 0.3){
            if (Q >= 0 & Q <= 52){los <- 'A'}
            if (Q > 52 & Q <= 77){los <- 'B'}
            if (Q > 77 & Q <= 129){los <- 'C'}
            if (Q > 129 & Q <= 194){los <- 'D'}
            if (Q > 194 & Q <= 252){los <- 'E'}
            if (Q > 252){los <- 'F'}
          }
          if (p_b == 0.4){
            if (Q >= 0 & Q <= 57){los <- 'A'}
            if (Q > 57 & Q <= 86){los <- 'B'}
            if (Q > 86 & Q <= 143){los <- 'C'}
            if (Q > 143 & Q <= 214){los <- 'D'}
            if (Q > 214 & Q <= 279){los <- 'E'}
            if (Q > 279){los <- 'F'}
          }
          if (p_b == 0.5){
            if (Q >= 0 & Q <= 64){los <- 'A'}
            if (Q > 64 & Q <= 96){los <- 'B'}
            if (Q > 96 & Q <= 160){los <- 'C'}
            if (Q > 160 & Q <= 240){los <- 'D'}
            if (Q > 240 & Q <= 312){los <- 'E'}
            if (Q > 312){los <- 'F'}
          }
          if (p_b == 0.6){
            if (Q >= 0 & Q <= 73){los <- 'A'}
            if (Q > 73 & Q <= 109){los <- 'B'}
            if (Q > 109 & Q <= 182){los <- 'C'}
            if (Q > 182 & Q <= 273){los <- 'D'}
            if (Q > 273 & Q <= 355){los <- 'E'}
            if (Q > 355){los <- 'F'}
          }
          if (p_b == 0.7){
            if (Q >= 0 & Q <= 84){los <- 'A'}
            if (Q > 84 & Q <= 126){los <- 'B'}
            if (Q > 126 & Q <= 211){los <- 'C'}
            if (Q > 211 & Q <= 316){los <- 'D'}
            if (Q > 316 & Q <= 411){los <- 'E'}
            if (Q > 411){los <- 'F'}
          }
        }
      }
      if (width == 3){
        if (is.null(n_conflict) == FALSE){
          if (n_conflict >= 0 & n_conflict <= 90){los <- 'A'}
          if (n_conflict > 90 & n_conflict <= 140){los <- 'B'}
          if (n_conflict > 140 & n_conflict <= 210){los <- 'C'}
          if (n_conflict > 210 & n_conflict <= 300){los <- 'D'}
          if (n_conflict > 300 & n_conflict <= 375){los <- 'E'}
          if (n_conflict > 375){los <- 'F'}
        }
        else{
          if (p_b == 0.3){
            if (Q >= 0 & Q <= 116){los <- 'A'}
            if (Q > 116 & Q <= 185){los <- 'B'}
            if (Q > 185 & Q <= 271){los <- 'C'}
            if (Q > 271 & Q <= 387){los <- 'D'}
            if (Q > 387 & Q <= 484){los <- 'E'}
            if (Q > 484){los <- 'F'}
          }
          if (p_b == 0.4){
            if (Q >= 0 & Q <= 129){los <- 'A'}
            if (Q > 129 & Q <= 200){los <- 'B'}
            if (Q > 200 & Q <= 300){los <- 'C'}
            if (Q > 300 & Q <= 429){los <- 'D'}
            if (Q > 429 & Q <= 536){los <- 'E'}
            if (Q > 536){los <- 'F'}
          }
          if (p_b == 0.5){
            if (Q >= 0 & Q <= 144){los <- 'A'}
            if (Q > 144 & Q <= 224){los <- 'B'}
            if (Q > 224 & Q <= 336){los <- 'C'}
            if (Q > 336 & Q <= 480){los <- 'D'}
            if (Q > 480 & Q <= 620){los <- 'E'}
            if (Q > 620){los <- 'F'}
          }
          if (p_b == 0.6){
            if (Q >= 0 & Q <= 164){los <- 'A'}
            if (Q > 164 & Q <= 255){los <- 'B'}
            if (Q > 255 & Q <= 382){los <- 'C'}
            if (Q > 382 & Q <= 545){los <- 'D'}
            if (Q > 545 & Q <= 682){los <- 'E'}
            if (Q > 682){los <- 'F'}
          }
          if (p_b == 0.7){
            if (Q >= 0 & Q <= 190){los <- 'A'}
            if (Q > 190 & Q <= 280){los <- 'B'}
            if (Q > 280 & Q <= 442){los <- 'C'}
            if (Q > 442 & Q <= 632){los <- 'D'}
            if (Q > 632 & Q <= 790){los <- 'E'}
            if (Q > 790){los <- 'F'}
          }
        }
      }
    }
  }
  if (type == 'bike_ped_comb'){
    if (width == 2){
      if (is.null(n_conflict) == FALSE){
        if (n_conflict >= 0 & n_conflict <= 40){los <- 'A'}
        if (n_conflict > 40 & n_conflict <= 60){los <- 'B'}
        if (n_conflict > 60 & n_conflict <= 100){los <- 'C'}
        if (n_conflict > 100 & n_conflict <= 150){los <- 'D'}
        if (n_conflict > 150 & n_conflict <= 195){los <- 'E'}
        if (n_conflict > 195){los <- 'F'}
      }
      else{
        if (Q >= 0 & Q <= 60){los <- 'A'}
        if (Q > 60 & Q <= 90){los <- 'B'}
        if (Q > 90 & Q <= 150){los <- 'C'}
        if (Q > 150 & Q <= 225){los <- 'D'}
        if (Q > 225 & Q <= 295){los <- 'E'}
        if (Q > 295){los <- 'F'}
      }
    }
    if (width == 3){
      if (is.null(n_conflict) == FALSE){
        if (n_conflict >= 0 & n_conflict <= 90){los <- 'A'}
        if (n_conflict > 90 & n_conflict <= 140){los <- 'B'}
        if (n_conflict > 140 & n_conflict <= 210){los <- 'C'}
        if (n_conflict > 210 & n_conflict <= 300){los <- 'D'}
        if (n_conflict > 300 & n_conflict <= 375){los <- 'E'}
        if (n_conflict > 375){los <- 'F'}
      }
      else{
        if (Q >= 0 & Q <= 135){los <- 'A'}
        if (Q > 135 & Q <= 210){los <- 'B'}
        if (Q > 210 & Q <= 315){los <- 'C'}
        if (Q > 315 & Q <= 450){los <- 'D'}
        if (Q > 450 & Q <= 565){los <- 'E'}
        if (Q > 565){los <- 'F'}
      }
    }
  }
  if (type == 'road_basic'){
    if (width == 2){
      if (is.null(n_conflict) == FALSE){
        if (n_conflict >= 0 & n_conflict <= 25){los <- 'A'}
        if (n_conflict > 25 & n_conflict <= 50){los <- 'B'}
        if (n_conflict > 50 & n_conflict <= 100){los <- 'C'}
        if (n_conflict > 100 & n_conflict <= 180){los <- 'D'}
        if (n_conflict > 180 & n_conflict <= 240){los <- 'E'}
        if (n_conflict > 240){los <- 'F'}
      }
      else{
        if (Q >= 0 & Q <= 100){los <- 'A'}
        if (Q > 100 & Q <= 200){los <- 'B'}
        if (Q > 200 & Q <= 400){los <- 'C'}
        if (Q > 400 & Q <= 720){los <- 'D'}
        if (Q > 720 & Q <= 960){los <- 'E'}
        if (Q > 960){los <- 'F'}
      }
    }
    if (width == 3){
      if (is.null(n_conflict) == FALSE){
        if (n_conflict >= 0 & n_conflict <= 150){los <- 'A'}
        if (n_conflict > 150 & n_conflict <= 295){los <- 'B'}
        if (n_conflict > 295 & n_conflict <= 590){los <- 'C'}
        if (n_conflict > 590 & n_conflict <= 1030){los <- 'D'}
        if (n_conflict > 1030 & n_conflict <= 1470){los <- 'E'}
        if (n_conflict > 1470){los <- 'F'}
      }
      else{
        if (Q >= 0 & Q <= 600){los <- 'A'}
        if (Q > 600 & Q <= 1180){los <- 'B'}
        if (Q > 1180 & Q <= 2360){los <- 'C'}
        if (Q > 2360 & Q <= 4120){los <- 'D'}
        if (Q > 4120 & Q <= 5880){los <- 'E'}
        if (Q > 5880){los <- 'F'}
      }
    }
  }
  los
}
