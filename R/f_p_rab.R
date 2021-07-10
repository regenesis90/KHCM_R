#' Pedestrian Crossing Influence Coefficient at Roundabout
#'
#' Pedestrian crossing influence coefficient at roundabout.
#'     It follows <Table 11-3> in KHCM(2013) p.498.
#' @param lane roundabout type. Choose one from : \code{1}, \code{2}
#' @param v_c Conflict Traffic Volume(pcph)
#' @param p_v Amount of passenger (person/hour)
#' @seealso \code{\link{capa_rab}}
#' @export f_p
#' @examples
#' f_p_rab(lane = 1, v_c = 1232, p_v = 323)
f_p_rab <- function(lane = NULL, v_c = NULL, p_v = NULL){
  if (lane == 1){
    if (p_v >= 0 & p_v <= 50){
      if (v_c >= 0 & v_c <= 100){f <- 1.0}
      if (v_c >= 101 & v_c <= 200){f <- 1.0}
      if (v_c >= 201 & v_c <= 300){f <- 1.0}
      if (v_c >= 301 & v_c <= 400){f <- 1.0}
      if (v_c >= 401 & v_c <= 500){f <- 1.0}
      if (v_c >= 501 & v_c <= 600){f <- 1.0}
      if (v_c >= 601 & v_c <= 700){f <- 1.0}
      if (v_c >= 701 & v_c <= 800){f <- 1.0}
      if (v_c >= 801 & v_c <= 900){f <- 1.0}
      if (v_c >= 901 & v_c <= 1000){f <- 1.0}
      if (v_c >= 1001 & v_c <= 1100){f <- 1.0}
      if (v_c >= 1101 & v_c <= 1200){f <- 1.0}
      if (v_c >= 1201 & v_c <= 1300){f <- 1.0}
      if (v_c >= 1301 & v_c <= 1400){f <- 1.0}
      if (v_c >= 1401){f <- 1.0}
    }
    else if (p_v >= 51 & p_v <= 150){
      if (v_c >= 0 & v_c <= 100){f <- 0.9}
      else if (v_c >= 101 & v_c <= 200){f <- 0.9}
      else if (v_c >= 201 & v_c <= 300){f <- 0.9}
      else if (v_c >= 301 & v_c <= 400){f <- 0.9}
      else if (v_c >= 401 & v_c <= 500){f <- 0.9}
      else if (v_c >= 501 & v_c <= 600){f <- 0.9}
      else if (v_c >= 601 & v_c <= 700){f <- 0.9}
      else if (v_c >= 701 & v_c <= 800){f <- 1.0}
      else if (v_c >= 801 & v_c <= 900){f <- 1.0}
      else if (v_c >= 901 & v_c <= 1000){f <- 1.0}
      else if (v_c >= 1001 & v_c <= 1100){f <- 1.0}
      else if (v_c >= 1101 & v_c <= 1200){f <- 1.0}
      else if (v_c >= 1201 & v_c <= 1300){f <- 1.0}
      else if (v_c >= 1301 & v_c <= 1400){f <- 1.0}
      else if (v_c >= 1401){f <- 1.0}
      else {f <- 'Error : [v_c] must be positive(pcph). Please check that.'}
    }
    else if (p_v >= 151 & p_v <= 250){
      if (v_c >= 0 & v_c <= 100){f <- 0.8}
      else if (v_c >= 101 & v_c <= 200){f <- 0.8}
      else if (v_c >= 201 & v_c <= 300){f <- 0.8}
      else if (v_c >= 301 & v_c <= 400){f <- 0.8}
      else if (v_c >= 401 & v_c <= 500){f <- 0.8}
      else if (v_c >= 501 & v_c <= 600){f <- 0.9}
      else if (v_c >= 601 & v_c <= 700){f <- 0.9}
      else if (v_c >= 701 & v_c <= 800){f <- 0.9}
      else if (v_c >= 801 & v_c <= 900){f <- 1.0}
      else if (v_c >= 901 & v_c <= 1000){f <- 1.0}
      else if (v_c >= 1001 & v_c <= 1100){f <- 1.0}
      else if (v_c >= 1101 & v_c <= 1200){f <- 1.0}
      else if (v_c >= 1201 & v_c <= 1300){f <- 1.0}
      else if (v_c >= 1301 & v_c <= 1400){f <- 1.0}
      else if (v_c >= 1401){f <- 1.0}
      else {f <- 'Error : [v_c] must be positive(pcph). Please check that.'}
    }
    else if (p_v >= 251 & p_v <= 350){
      if (v_c >= 0 & v_c <= 100){f <- 0.7}
      else if (v_c >= 101 & v_c <= 200){f <- 0.7}
      else if (v_c >= 201 & v_c <= 300){f <- 0.7}
      else if (v_c >= 301 & v_c <= 400){f <- 0.7}
      else if (v_c >= 401 & v_c <= 500){f <- 0.8}
      else if (v_c >= 501 & v_c <= 600){f <- 0.8}
      else if (v_c >= 601 & v_c <= 700){f <- 0.8}
      else if (v_c >= 701 & v_c <= 800){f <- 0.9}
      else if (v_c >= 801 & v_c <= 900){f <- 1.0}
      else if (v_c >= 901 & v_c <= 1000){f <- 1.0}
      else if (v_c >= 1001 & v_c <= 1100){f <- 1.0}
      else if (v_c >= 1101 & v_c <= 1200){f <- 1.0}
      else if (v_c >= 1201 & v_c <= 1300){f <- 1.0}
      else if (v_c >= 1301 & v_c <= 1400){f <- 1.0}
      else if (v_c >= 1401){f <- 1.0}
      else {f <- 'Error : [v_c] must be positive(pcph). Please check that.'}
    }
    else if (p_v >= 351){
      if (v_c >= 0 & v_c <= 100){f <- 0.6}
      else if (v_c >= 101 & v_c <= 200){f <- 0.6}
      else if (v_c >= 201 & v_c <= 300){f <- 0.7}
      else if (v_c >= 301 & v_c <= 400){f <- 0.7}
      else if (v_c >= 401 & v_c <= 500){f <- 0.7}
      else if (v_c >= 501 & v_c <= 600){f <- 0.8}
      else if (v_c >= 601 & v_c <= 700){f <- 0.8}
      else if (v_c >= 701 & v_c <= 800){f <- 0.9}
      else if (v_c >= 801 & v_c <= 900){f <- 0.9}
      else if (v_c >= 901 & v_c <= 1000){f <- 1.0}
      else if (v_c >= 1001 & v_c <= 1100){f <- 1.0}
      else if (v_c >= 1101 & v_c <= 1200){f <- 1.0}
      else if (v_c >= 1201 & v_c <= 1300){f <- 1.0}
      else if (v_c >= 1301 & v_c <= 1400){f <- 1.0}
      else if (v_c >= 1401){f <- 1.0}
      else {f <- 'Error : [v_c] must be positive(pcph). Please check that.'}
    }
    else {f <- 'Error : [p_v] must be positive(persons/hour). Please check that.'}
  }
  else if (lane == 2){
    if (p_v >= 0 & p_v <= 50){
      if (v_c >= 0 & v_c <= 100){f <- 1.0}
      else if (v_c >= 101 & v_c <= 200){f <- 1.0}
      else if (v_c >= 201 & v_c <= 300){f <- 1.0}
      else if (v_c >= 301 & v_c <= 400){f <- 1.0}
      else if (v_c >= 401 & v_c <= 500){f <- 1.0}
      else if (v_c >= 501 & v_c <= 600){f <- 1.0}
      else if (v_c >= 601 & v_c <= 700){f <- 1.0}
      else if (v_c >= 701 & v_c <= 800){f <- 1.0}
      else if (v_c >= 801 & v_c <= 900){f <- 1.0}
      else if (v_c >= 901 & v_c <= 1000){f <- 1.0}
      else if (v_c >= 1001 & v_c <= 1100){f <- 1.0}
      else if (v_c >= 1101 & v_c <= 1200){f <- 1.0}
      else if (v_c >= 1201 & v_c <= 1300){f <- 1.0}
      else if (v_c >= 1301 & v_c <= 1400){f <- 1.0}
      else if (v_c >= 1401){f <- 1.0}
      else {f <- 'Error : [v_c] must be positive(pcph). Please check that.'}
    }
    else if (p_v >= 51 & p_v <= 150){
      if (v_c >= 0 & v_c <= 100){f <- 0.9}
      else if (v_c >= 101 & v_c <= 200){f <- 0.9}
      else if (v_c >= 201 & v_c <= 300){f <- 0.9}
      else if (v_c >= 301 & v_c <= 400){f <- 0.9}
      else if (v_c >= 401 & v_c <= 500){f <- 0.9}
      else if (v_c >= 501 & v_c <= 600){f <- 0.9}
      else if (v_c >= 601 & v_c <= 700){f <- 0.9}
      else if (v_c >= 701 & v_c <= 800){f <- 0.9}
      else if (v_c >= 801 & v_c <= 900){f <- 0.9}
      else if (v_c >= 901 & v_c <= 1000){f <- 1.0}
      else if (v_c >= 1001 & v_c <= 1100){f <- 1.0}
      else if (v_c >= 1101 & v_c <= 1200){f <- 1.0}
      else if (v_c >= 1201 & v_c <= 1300){f <- 1.0}
      else if (v_c >= 1301 & v_c <= 1400){f <- 1.0}
      else if (v_c >= 1401){f <- 1.0}
      else {f <- 'Error : [v_c] must be positive(pcph). Please check that.'}
    }
    else if (p_v >= 151 & p_v <= 250){
      if (v_c >= 0 & v_c <= 100){f <- 0.8}
      else if (v_c >= 101 & v_c <= 200){f <- 0.8}
      else if (v_c >= 201 & v_c <= 300){f <- 0.8}
      else if (v_c >= 301 & v_c <= 400){f <- 0.8}
      else if (v_c >= 401 & v_c <= 500){f <- 0.8}
      else if (v_c >= 501 & v_c <= 600){f <- 0.8}
      else if (v_c >= 601 & v_c <= 700){f <- 0.8}
      else if (v_c >= 701 & v_c <= 800){f <- 0.8}
      else if (v_c >= 801 & v_c <= 900){f <- 0.8}
      else if (v_c >= 901 & v_c <= 1000){f <- 0.8}
      else if (v_c >= 1001 & v_c <= 1100){f <- 0.9}
      else if (v_c >= 1101 & v_c <= 1200){f <- 1.0}
      else if (v_c >= 1201 & v_c <= 1300){f <- 1.0}
      else if (v_c >= 1301 & v_c <= 1400){f <- 1.0}
      else if (v_c >= 1401){f <- 1.0}
      else {f <- 'Error : [v_c] must be positive(pcph). Please check that.'}
    }
    else if (p_v >= 251 & p_v <= 350){
      if (v_c >= 0 & v_c <= 100){f <- 0.7}
      else if (v_c >= 101 & v_c <= 200){f <- 0.7}
      else if (v_c >= 201 & v_c <= 300){f <- 0.7}
      else if (v_c >= 301 & v_c <= 400){f <- 0.7}
      else if (v_c >= 401 & v_c <= 500){f <- 0.7}
      else if (v_c >= 501 & v_c <= 600){f <- 0.7}
      else if (v_c >= 601 & v_c <= 700){f <- 0.7}
      else if (v_c >= 701 & v_c <= 800){f <- 0.8}
      else if (v_c >= 801 & v_c <= 900){f <- 0.8}
      else if (v_c >= 901 & v_c <= 1000){f <- 0.8}
      else if (v_c >= 1001 & v_c <= 1100){f <- 0.9}
      else if (v_c >= 1101 & v_c <= 1200){f <- 0.9}
      else if (v_c >= 1201 & v_c <= 1300){f <- 0.9}
      else if (v_c >= 1301 & v_c <= 1400){f <- 1.0}
      else if (v_c >= 1401){f <- 1.0}
      else {f <- 'Error : [v_c] must be positive(pcph). Please check that.'}
    }
    else if (p_v >= 351){
      if (v_c >= 0 & v_c <= 100){f <- 0.6}
      else if (v_c >= 101 & v_c <= 200){f <- 0.6}
      else if (v_c >= 201 & v_c <= 300){f <- 0.6}
      else if (v_c >= 301 & v_c <= 400){f <- 0.6}
      else if (v_c >= 401 & v_c <= 500){f <- 0.6}
      else if (v_c >= 501 & v_c <= 600){f <- 0.6}
      else if (v_c >= 601 & v_c <= 700){f <- 0.6}
      else if (v_c >= 701 & v_c <= 800){f <- 0.7}
      else if (v_c >= 801 & v_c <= 900){f <- 0.7}
      else if (v_c >= 901 & v_c <= 1000){f <- 0.7}
      else if (v_c >= 1001 & v_c <= 1100){f <- 0.8}
      else if (v_c >= 1101 & v_c <= 1200){f <- 0.8}
      else if (v_c >= 1201 & v_c <= 1300){f <- 0.8}
      else if (v_c >= 1301 & v_c <= 1400){f <- 0.9}
      else if (v_c >= 1401){f <- 1.0}
      else {f <- 'Error : [v_c] must be positive(pcph). Please check that.'}
    }
    else {f <- 'Error : [p_v] must be positive(persons/hour). Please check that.'}
  }
  else {f <- 'Error : [lane] must be one of 1, 2. Please check that.'}
  f
}
