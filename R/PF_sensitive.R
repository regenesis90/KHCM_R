#' Interlocking coefficient of response signal(PF_sensitive)
#'
#' It follows <Table 12-9> in KHCM(2013) p.542
#' @param type Signal Type. Choose one from: \code{'sensitive'}, \code{'semi-sensitive'}
#' @param direction \code{'main'}, \code{'sub'}
#' @param turn \code{'straight'}, \code{'right'}, \code{'simultaneous_left'}, \code{'dedicated_left'}
#' @param arrival_type Choose one from: \code{1}, \code{2}, \code{3}, \code{4}, \code{5}
#' @param v_c_ratio
#' @export PF_sensitive
#' @examples
#' PF_sensitive('semi-sensitive', 'main', 'right', 3, 0.23)
PF_sensitive <- function(type = NULL, direction = NULL, turn = NULL, arrival_type = NULL, v_c_ratio = NULL){
  if (type == 'sensitive'){
    if (turn == 'straight'){
      if (v_c_ratio <= 0.6){
        if (arrival_type == 1){f <- 1.54}
        if (arrival_type == 2){f <- 1.08}
        if (arrival_type == 3){f <- 0.85}
        if (arrival_type == 4){f <- 0.62}
        if (arrival_type == 5){f <- 0.40}
      }
    }
    if (turn == 'right'){
      if (v_c_ratio <= 0.8){
        if (arrival_type == 1){f <- 1.25}
        if (arrival_type == 2){f <- 0.98}
        if (arrival_type == 3){f <- 0.85}
        if (arrival_type == 4){f <- 0.71}
        if (arrival_type == 5){f <- 0.50}
      }
    }
    if (turn == 'simultaneous_left'){
      if (v_c_ratio <= 1.0){
        if (arrival_type == 1){f <- 1.16}
        if (arrival_type == 2){f <- 0.94}
        if (arrival_type == 3){f <- 0.85}
        if (arrival_type == 4){f <- 0.78}
        if (arrival_type == 5){f <- 0.61}
      }
    }
    if (turn == 'dedicated_left'){f <- 1.0}
    }
  if (type == 'semi-sensitive'){
    if (direction == 'main'){
      if (turn == 'straight'){
        if (v_c_ratio <= 0.6){
          if (arrival_type == 1){f <- 1.85}
          if (arrival_type == 2){f <- 1.35}
          if (arrival_type == 3){f <- 1.00}
          if (arrival_type == 4){f <- 0.72}
          if (arrival_type == 5){f <- 0.42}
        }
      }
      if (turn == 'right'){
        if (v_c_ratio <= 0.8){
          if (arrival_type == 1){f <- 1.50}
          if (arrival_type == 2){f <- 1.22}
          if (arrival_type == 3){f <- 1.00}
          if (arrival_type == 4){f <- 0.82}
          if (arrival_type == 5){f <- 0.53}
        }
      }
      if (turn == 'simultaneous_left'){
        if (v_c_ratio <= 1.0){
          if (arrival_type == 1){f <- 1.40}
          if (arrival_type == 2){f <- 1.18}
          if (arrival_type == 3){f <- 1.00}
          if (arrival_type == 4){f <- 0.90}
          if (arrival_type == 5){f <- 0.65}
        }
      }
      if (turn == 'dedicated_left'){f <- 1.0}
    }
    if (direction == 'sub'){
      if (turn == 'straight'){
        if (v_c_ratio <= 0.6){
          if (arrival_type == 1){f <- 1.48}
          if (arrival_type == 2){f <- 1.18}
          if (arrival_type == 3){f <- 1.00}
          if (arrival_type == 4){f <- 0.86}
          if (arrival_type == 5){f <- 0.70}
        }
      }
      if (turn == 'right'){
        if (v_c_ratio <= 0.8){
          if (arrival_type == 1){f <- 1.20}
          if (arrival_type == 2){f <- 1.07}
          if (arrival_type == 3){f <- 1.00}
          if (arrival_type == 4){f <- 0.98}
          if (arrival_type == 5){f <- 0.89}
        }
      }
      if (turn == 'simultaneous_left'){
        if (v_c_ratio <= 1.0){
          if (arrival_type == 1){f <- 1.12}
          if (arrival_type == 2){f <- 1.04}
          if (arrival_type == 3){f <- 1.00}
          if (arrival_type == 4){f <- 1.00}
          if (arrival_type == 5){f <- 1.00}
        }
      }
      if (turn == 'dedicated_left'){f <- 1.0}
    }
  }
  f
}
