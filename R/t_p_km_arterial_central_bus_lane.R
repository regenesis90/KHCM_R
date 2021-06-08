#' Section travel time per km in Arterial Road with Central Bus lane(t_p_km_arterial_central_bus_lane, seconds/km)
#'
#' It follows <Table 12-11> in KHCM(2013) p.550
#' @param L Interval between analysis sections (km)
#' @param overtaking_lane Choose one from: \code{'yes'}, \code{'no'}
#' @param N_bus_stop Number of bus stop. \code{0}, \code{1}, \code{2}
#' @keywords
#' @export t_p_km_arterial_central_bus_lane
#' @examples
#' t_p_km_arterial_central_bus_lane(0.32, 'yes', 1)
t_p_km_arterial_central_bus_lane <- function(L = NULL, overtaking_lane = NULL, N_bus_stop = NULL){
  if (overtaking_lane == 'yes'){
    if (N_bus_stop == 1){
      if (L > 0 & L <= 0.1){tpk <- 316}
      if (L > 0.1 & L <= 0.2){tpk <- 198}
      if (L > 0.2 & L <= 0.3){tpk <- 163}
      if (L > 0.3 & L <= 0.4){tpk <- 144}
      if (L > 0.4 & L <= 0.5){tpk <- 133}
      if (L > 0.5 & L <= 0.6){tpk <- 124}
      if (L > 0.6 & L <= 0.7){tpk <- 118}
      if (L > 0.7 & L <= 0.8){tpk <- 113}
      if (L > 0.8 & L <= 0.9){tpk <- 109}
      if (L > 0.9){tpk <- 106}
    }
    if (N_bus_stop == 2){
      if (L > 0.5 & L <= 0.6){tpk <- 157}
      if (L > 0.6 & L <= 0.7){tpk <- 150}
      if (L > 0.7 & L <= 0.8){tpk <- 145}
      if (L > 0.8 & L <= 0.9){tpk <- 140}
      if (L > 0.9){tpk <- 136}
    }
  }
  if (overtaking_lane == 'no'){
    if (N_bus_stop == 0){
      if (L > 0 & L <= 0.1){tpk <- 91}
      if (L > 0.1 & L <= 0.2){tpk <- 78}
      if (L > 0.2 & L <= 0.3){tpk <- 73}
      if (L > 0.3 & L <= 0.4){tpk <- 69}
      if (L > 0.4 & L <= 0.5){tpk <- 66}
      if (L > 0.5 & L <= 0.6){tpk <- 64}
      if (L > 0.6 & L <= 0.7){tpk <- 63}
      if (L > 0.7 & L <= 0.8){tpk <- 61}
      if (L > 0.8 & L <= 0.9){tpk <- 60}
      if (L > 0.9){tpk <- 59}
    }
    if (N_bus_stop == 1){
      if (L > 0 & L <= 0.1){tpk <- 374}
      if (L > 0.1 & L <= 0.2){tpk <- 223}
      if (L > 0.2 & L <= 0.3){tpk <- 181}
      if (L > 0.3 & L <= 0.4){tpk <- 159}
      if (L > 0.4 & L <= 0.5){tpk <- 146}
      if (L > 0.5 & L <= 0.6){tpk <- 136}
      if (L > 0.6 & L <= 0.7){tpk <- 129}
      if (L > 0.7 & L <= 0.8){tpk <- 124}
      if (L > 0.8 & L <= 0.9){tpk <- 119}
      if (L > 0.9){tpk <- 116}
    }
    if (N_bus_stop == 2){
      if (L > 0.5 & L <= 0.6){tpk <- 175}
      if (L > 0.6 & L <= 0.7){tpk <- 168}
      if (L > 0.7 & L <= 0.8){tpk <- 162}
      if (L > 0.8 & L <= 0.9){tpk <- 158}
      if (L > 0.9){tpk <- 154}
    }
  }
  tpk
}
