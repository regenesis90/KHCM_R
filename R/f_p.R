#' Influence coefficient on the number of pedestrians by conflicting traffic volume(f_p)
#'
#' It follows <Table 11-3> in KHCM(2013) p.498
#' @param lane roundabout type. Choose one from : \code{1}, \code{2}
#' @param V_ci Conflict Traffic Volume(pcph)
#' @param p_v Amount of passenger (person/hour)
#' @export f_p
#' @examples
#' f_p(1, 1232, 323)
#' f_p(2, 546, 215)
f_p <- function(lane = NULL, V_ci = NULL, p_v = NULL){
  if (lane == 1){
    if (p_v >= 0 & p_v <= 50){
      if (V_ci >= 0 & V_ci <= 100){f <- 1.0}
      if (V_ci >= 101 & V_ci <= 200){f <- 1.0}
      if (V_ci >= 201 & V_ci <= 300){f <- 1.0}
      if (V_ci >= 301 & V_ci <= 400){f <- 1.0}
      if (V_ci >= 401 & V_ci <= 500){f <- 1.0}
      if (V_ci >= 501 & V_ci <= 600){f <- 1.0}
      if (V_ci >= 601 & V_ci <= 700){f <- 1.0}
      if (V_ci >= 701 & V_ci <= 800){f <- 1.0}
      if (V_ci >= 801 & V_ci <= 900){f <- 1.0}
      if (V_ci >= 901 & V_ci <= 1000){f <- 1.0}
      if (V_ci >= 1001 & V_ci <= 1100){f <- 1.0}
      if (V_ci >= 1101 & V_ci <= 1200){f <- 1.0}
      if (V_ci >= 1201 & V_ci <= 1300){f <- 1.0}
      if (V_ci >= 1301 & V_ci <= 1400){f <- 1.0}
      if (V_ci >= 1401){f <- 1.0}
    }
    if (p_v >= 51 & p_v <= 150){
      if (V_ci >= 0 & V_ci <= 100){f <- 0.9}
      if (V_ci >= 101 & V_ci <= 200){f <- 0.9}
      if (V_ci >= 201 & V_ci <= 300){f <- 0.9}
      if (V_ci >= 301 & V_ci <= 400){f <- 0.9}
      if (V_ci >= 401 & V_ci <= 500){f <- 0.9}
      if (V_ci >= 501 & V_ci <= 600){f <- 0.9}
      if (V_ci >= 601 & V_ci <= 700){f <- 0.9}
      if (V_ci >= 701 & V_ci <= 800){f <- 1.0}
      if (V_ci >= 801 & V_ci <= 900){f <- 1.0}
      if (V_ci >= 901 & V_ci <= 1000){f <- 1.0}
      if (V_ci >= 1001 & V_ci <= 1100){f <- 1.0}
      if (V_ci >= 1101 & V_ci <= 1200){f <- 1.0}
      if (V_ci >= 1201 & V_ci <= 1300){f <- 1.0}
      if (V_ci >= 1301 & V_ci <= 1400){f <- 1.0}
      if (V_ci >= 1401){f <- 1.0}
    }
    if (p_v >= 151 & p_v <= 250){
      if (V_ci >= 0 & V_ci <= 100){f <- 0.8}
      if (V_ci >= 101 & V_ci <= 200){f <- 0.8}
      if (V_ci >= 201 & V_ci <= 300){f <- 0.8}
      if (V_ci >= 301 & V_ci <= 400){f <- 0.8}
      if (V_ci >= 401 & V_ci <= 500){f <- 0.8}
      if (V_ci >= 501 & V_ci <= 600){f <- 0.9}
      if (V_ci >= 601 & V_ci <= 700){f <- 0.9}
      if (V_ci >= 701 & V_ci <= 800){f <- 0.9}
      if (V_ci >= 801 & V_ci <= 900){f <- 1.0}
      if (V_ci >= 901 & V_ci <= 1000){f <- 1.0}
      if (V_ci >= 1001 & V_ci <= 1100){f <- 1.0}
      if (V_ci >= 1101 & V_ci <= 1200){f <- 1.0}
      if (V_ci >= 1201 & V_ci <= 1300){f <- 1.0}
      if (V_ci >= 1301 & V_ci <= 1400){f <- 1.0}
      if (V_ci >= 1401){f <- 1.0}
    }
    if (p_v >= 251 & p_v <= 350){
      if (V_ci >= 0 & V_ci <= 100){f <- 0.7}
      if (V_ci >= 101 & V_ci <= 200){f <- 0.7}
      if (V_ci >= 201 & V_ci <= 300){f <- 0.7}
      if (V_ci >= 301 & V_ci <= 400){f <- 0.7}
      if (V_ci >= 401 & V_ci <= 500){f <- 0.8}
      if (V_ci >= 501 & V_ci <= 600){f <- 0.8}
      if (V_ci >= 601 & V_ci <= 700){f <- 0.8}
      if (V_ci >= 701 & V_ci <= 800){f <- 0.9}
      if (V_ci >= 801 & V_ci <= 900){f <- 1.0}
      if (V_ci >= 901 & V_ci <= 1000){f <- 1.0}
      if (V_ci >= 1001 & V_ci <= 1100){f <- 1.0}
      if (V_ci >= 1101 & V_ci <= 1200){f <- 1.0}
      if (V_ci >= 1201 & V_ci <= 1300){f <- 1.0}
      if (V_ci >= 1301 & V_ci <= 1400){f <- 1.0}
      if (V_ci >= 1401){f <- 1.0}
    }
    if (p_v >= 351){
      if (V_ci >= 0 & V_ci <= 100){f <- 0.6}
      if (V_ci >= 101 & V_ci <= 200){f <- 0.6}
      if (V_ci >= 201 & V_ci <= 300){f <- 0.7}
      if (V_ci >= 301 & V_ci <= 400){f <- 0.7}
      if (V_ci >= 401 & V_ci <= 500){f <- 0.7}
      if (V_ci >= 501 & V_ci <= 600){f <- 0.8}
      if (V_ci >= 601 & V_ci <= 700){f <- 0.8}
      if (V_ci >= 701 & V_ci <= 800){f <- 0.9}
      if (V_ci >= 801 & V_ci <= 900){f <- 0.9}
      if (V_ci >= 901 & V_ci <= 1000){f <- 1.0}
      if (V_ci >= 1001 & V_ci <= 1100){f <- 1.0}
      if (V_ci >= 1101 & V_ci <= 1200){f <- 1.0}
      if (V_ci >= 1201 & V_ci <= 1300){f <- 1.0}
      if (V_ci >= 1301 & V_ci <= 1400){f <- 1.0}
      if (V_ci >= 1401){f <- 1.0}
    }
  }
  if (lane == 2){
    if (p_v >= 0 & p_v <= 50){
      if (V_ci >= 0 & V_ci <= 100){f <- 1.0}
      if (V_ci >= 101 & V_ci <= 200){f <- 1.0}
      if (V_ci >= 201 & V_ci <= 300){f <- 1.0}
      if (V_ci >= 301 & V_ci <= 400){f <- 1.0}
      if (V_ci >= 401 & V_ci <= 500){f <- 1.0}
      if (V_ci >= 501 & V_ci <= 600){f <- 1.0}
      if (V_ci >= 601 & V_ci <= 700){f <- 1.0}
      if (V_ci >= 701 & V_ci <= 800){f <- 1.0}
      if (V_ci >= 801 & V_ci <= 900){f <- 1.0}
      if (V_ci >= 901 & V_ci <= 1000){f <- 1.0}
      if (V_ci >= 1001 & V_ci <= 1100){f <- 1.0}
      if (V_ci >= 1101 & V_ci <= 1200){f <- 1.0}
      if (V_ci >= 1201 & V_ci <= 1300){f <- 1.0}
      if (V_ci >= 1301 & V_ci <= 1400){f <- 1.0}
      if (V_ci >= 1401){f <- 1.0}
    }
    if (p_v >= 51 & p_v <= 150){
      if (V_ci >= 0 & V_ci <= 100){f <- 0.9}
      if (V_ci >= 101 & V_ci <= 200){f <- 0.9}
      if (V_ci >= 201 & V_ci <= 300){f <- 0.9}
      if (V_ci >= 301 & V_ci <= 400){f <- 0.9}
      if (V_ci >= 401 & V_ci <= 500){f <- 0.9}
      if (V_ci >= 501 & V_ci <= 600){f <- 0.9}
      if (V_ci >= 601 & V_ci <= 700){f <- 0.9}
      if (V_ci >= 701 & V_ci <= 800){f <- 0.9}
      if (V_ci >= 801 & V_ci <= 900){f <- 0.9}
      if (V_ci >= 901 & V_ci <= 1000){f <- 1.0}
      if (V_ci >= 1001 & V_ci <= 1100){f <- 1.0}
      if (V_ci >= 1101 & V_ci <= 1200){f <- 1.0}
      if (V_ci >= 1201 & V_ci <= 1300){f <- 1.0}
      if (V_ci >= 1301 & V_ci <= 1400){f <- 1.0}
      if (V_ci >= 1401){f <- 1.0}
    }
    if (p_v >= 151 & p_v <= 250){
      if (V_ci >= 0 & V_ci <= 100){f <- 0.8}
      if (V_ci >= 101 & V_ci <= 200){f <- 0.8}
      if (V_ci >= 201 & V_ci <= 300){f <- 0.8}
      if (V_ci >= 301 & V_ci <= 400){f <- 0.8}
      if (V_ci >= 401 & V_ci <= 500){f <- 0.8}
      if (V_ci >= 501 & V_ci <= 600){f <- 0.8}
      if (V_ci >= 601 & V_ci <= 700){f <- 0.8}
      if (V_ci >= 701 & V_ci <= 800){f <- 0.8}
      if (V_ci >= 801 & V_ci <= 900){f <- 0.8}
      if (V_ci >= 901 & V_ci <= 1000){f <- 0.8}
      if (V_ci >= 1001 & V_ci <= 1100){f <- 0.9}
      if (V_ci >= 1101 & V_ci <= 1200){f <- 1.0}
      if (V_ci >= 1201 & V_ci <= 1300){f <- 1.0}
      if (V_ci >= 1301 & V_ci <= 1400){f <- 1.0}
      if (V_ci >= 1401){f <- 1.0}
    }
    if (p_v >= 251 & p_v <= 350){
      if (V_ci >= 0 & V_ci <= 100){f <- 0.7}
      if (V_ci >= 101 & V_ci <= 200){f <- 0.7}
      if (V_ci >= 201 & V_ci <= 300){f <- 0.7}
      if (V_ci >= 301 & V_ci <= 400){f <- 0.7}
      if (V_ci >= 401 & V_ci <= 500){f <- 0.7}
      if (V_ci >= 501 & V_ci <= 600){f <- 0.7}
      if (V_ci >= 601 & V_ci <= 700){f <- 0.7}
      if (V_ci >= 701 & V_ci <= 800){f <- 0.8}
      if (V_ci >= 801 & V_ci <= 900){f <- 0.8}
      if (V_ci >= 901 & V_ci <= 1000){f <- 0.8}
      if (V_ci >= 1001 & V_ci <= 1100){f <- 0.9}
      if (V_ci >= 1101 & V_ci <= 1200){f <- 0.9}
      if (V_ci >= 1201 & V_ci <= 1300){f <- 0.9}
      if (V_ci >= 1301 & V_ci <= 1400){f <- 1.0}
      if (V_ci >= 1401){f <- 1.0}
    }
    if (p_v >= 351){
      if (V_ci >= 0 & V_ci <= 100){f <- 0.6}
      if (V_ci >= 101 & V_ci <= 200){f <- 0.6}
      if (V_ci >= 201 & V_ci <= 300){f <- 0.6}
      if (V_ci >= 301 & V_ci <= 400){f <- 0.6}
      if (V_ci >= 401 & V_ci <= 500){f <- 0.6}
      if (V_ci >= 501 & V_ci <= 600){f <- 0.6}
      if (V_ci >= 601 & V_ci <= 700){f <- 0.6}
      if (V_ci >= 701 & V_ci <= 800){f <- 0.7}
      if (V_ci >= 801 & V_ci <= 900){f <- 0.7}
      if (V_ci >= 901 & V_ci <= 1000){f <- 0.7}
      if (V_ci >= 1001 & V_ci <= 1100){f <- 0.8}
      if (V_ci >= 1101 & V_ci <= 1200){f <- 0.8}
      if (V_ci >= 1201 & V_ci <= 1300){f <- 0.8}
      if (V_ci >= 1301 & V_ci <= 1400){f <- 0.9}
      if (V_ci >= 1401){f <- 1.0}
    }
  }
  f
}
