#' Section travel time per km(t_p_km_arterial, seconds/km)
#'
#' It follows <Table 12-5> in KHCM(2013) p.536
#' @param free_speed Choose one from : \code{80}, \code{70}, \code{60}
#' @param roadside_friction_arterial Choose one from: \code{'big'}, \code{'small'}. See roadside_friction_arterial()
#' @param L section length(km)
#' @keywords
#' @export t_p_km_arterial
#' @examples
#' t_p_km_arterial(free_speed = 80, roadside_friction_arterial = 'big', L = 0.83)
#' t_p_km_arterial(60, 'small', 0.213)
t_p_km_arterial <- function(free_speed = NULL, roadside_friction_arterial = NULL, L = NULL){
  if (free_speed == 80){
    if (roadside_friction_arterial == 'big'){
      if (L > 0 & L <= 0.1){tpk <- 108}
      if (L > 0.1 & L <= 0.2){tpk <- 80}
      if (L > 0.2 & L <= 0.3){tpk <- 71}
      if (L > 0.3 & L <= 0.4){tpk <- 66}
      if (L > 0.4 & L <= 0.5){tpk <- 63}
      if (L > 0.5 & L <= 0.6){tpk <- 61}
      if (L > 0.6 & L <= 0.7){tpk <- 60}
      if (L > 0.7 & L <= 0.8){tpk <- 59}
      if (L > 0.8 & L <= 0.9){tpk <- 58}
      if (L > 0.9){tpk <- 58}
    }
    if (roadside_friction_arterial == 'small'){
      if (L > 0 & L <= 0.1){tpk <- 86}
      if (L > 0.1 & L <= 0.2){tpk <- 66}
      if (L > 0.2 & L <= 0.3){tpk <- 59}
      if (L > 0.3 & L <= 0.4){tpk <- 56}
      if (L > 0.4 & L <= 0.5){tpk <- 54}
      if (L > 0.5 & L <= 0.6){tpk <- 53}
      if (L > 0.6 & L <= 0.7){tpk <- 52}
      if (L > 0.7 & L <= 0.8){tpk <- 51}
      if (L > 0.8 & L <= 0.9){tpk <- 50}
      if (L > 0.9){tpk <- 50}
    }
  }
  if (free_speed == 70){
    if (roadside_friction_arterial == 'big'){
      if (L > 0 & L <= 0.1){tpk <- 143}
      if (L > 0.1 & L <= 0.2){tpk <- 100}
      if (L > 0.2 & L <= 0.3){tpk <- 85}
      if (L > 0.3 & L <= 0.4){tpk <- 77}
      if (L > 0.4 & L <= 0.5){tpk <- 73}
      if (L > 0.5 & L <= 0.6){tpk <- 70}
      if (L > 0.6 & L <= 0.7){tpk <- 68}
      if (L > 0.7 & L <= 0.8){tpk <- 66}
      if (L > 0.8 & L <= 0.9){tpk <- 65}
      if (L > 0.9){tpk <- 65}
    }
    if (roadside_friction_arterial == 'small'){
      if (L > 0 & L <= 0.1){tpk <- 102}
      if (L > 0.1 & L <= 0.2){tpk <- 75}
      if (L > 0.2 & L <= 0.3){tpk <- 67}
      if (L > 0.3 & L <= 0.4){tpk <- 63}
      if (L > 0.4 & L <= 0.5){tpk <- 60}
      if (L > 0.5 & L <= 0.6){tpk <- 58}
      if (L > 0.6 & L <= 0.7){tpk <- 57}
      if (L > 0.7 & L <= 0.8){tpk <- 56}
      if (L > 0.8 & L <= 0.9){tpk <- 55}
      if (L > 0.9){tpk <- 54}
    }
  }
  if (free_speed == 60){
    if (roadside_friction_arterial == 'big'){
      if (L > 0 & L <= 0.1){tpk <- 178}
      if (L > 0.1 & L <= 0.2){tpk <- 119}
      if (L > 0.2 & L <= 0.3){tpk <- 99}
      if (L > 0.3 & L <= 0.4){tpk <- 88}
      if (L > 0.4 & L <= 0.5){tpk <- 83}
      if (L > 0.5 & L <= 0.6){tpk <- 79}
      if (L > 0.6 & L <= 0.7){tpk <- 75}
      if (L > 0.7 & L <= 0.8){tpk <- 74}
      if (L > 0.8 & L <= 0.9){tpk <- 72}
      if (L > 0.9){tpk <- 72}
    }
    if (roadside_friction_arterial == 'small'){
      if (L > 0 & L <= 0.1){tpk <- 119}
      if (L > 0.1 & L <= 0.2){tpk <- 85}
      if (L > 0.2 & L <= 0.3){tpk <- 74}
      if (L > 0.3 & L <= 0.4){tpk <- 69}
      if (L > 0.4 & L <= 0.5){tpk <- 65}
      if (L > 0.5 & L <= 0.6){tpk <- 63}
      if (L > 0.6 & L <= 0.7){tpk <- 62}
      if (L > 0.7 & L <= 0.8){tpk <- 61}
      if (L > 0.8 & L <= 0.9){tpk <- 60}
      if (L > 0.9){tpk <- 58}
    }
  }
  tpk
}
