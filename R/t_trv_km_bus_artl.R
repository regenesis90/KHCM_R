#' Section Travel Time Per km of Bus on Arterial Road with Exclusive Central Bus lane
#'
#' Section cruising time per km of a bus traveling on the exclusive central bus lane (sec/km).
#'     It follows <Table 12-11> in KHCM(2013) p.550.
#' @param L Interval between analyzed sections(km)
#' @param ovrtk_lane Existence of overtaking lane. Choose one from: \code{'yes'}, \code{'no'}
#' @param bus_stop Number of bus stop. \code{0}, \code{1}, \code{2}
#' @keywords section travel time bus arterial road exclusive central bus lane
#' @details
#'     - In this function, only the presence of a stop and an overtaking lane is considered as factors affecting the traveling time.
#'     - Since there were no cases where there were two stops within 500m of the analysis section,
#'     this was not taken into account. (According to the design guidelines for express buses in Korea, the interval between urban bus stops is at least 500m)
#' @seealso \code{\link{ATS_bus_seg_artl}}
#' @export t_trv_km_bus_artl
#' @examples
#' t_trv_km_bus_artl(L = 0.32, ovrtk_lane = 'yes', bus_stop = 1)
t_trv_km_bus_artl <- function(L = NULL, ovrtk_lane = NULL, bus_stop = NULL){
  if (ovrtk_lane == 'yes'){
    if (bus_stop == 1){
      if (L > 0 & L <= 0.1){tpk <- 316}
      else if (L > 0.1 & L <= 0.2){tpk <- 198}
      else if (L > 0.2 & L <= 0.3){tpk <- 163}
      else if (L > 0.3 & L <= 0.4){tpk <- 144}
      else if (L > 0.4 & L <= 0.5){tpk <- 133}
      else if (L > 0.5 & L <= 0.6){tpk <- 124}
      else if (L > 0.6 & L <= 0.7){tpk <- 118}
      else if (L > 0.7 & L <= 0.8){tpk <- 113}
      else if (L > 0.8 & L <= 0.9){tpk <- 109}
      else if (L > 0.9){tpk <- 106}
      else {tpk <- 'Error : [L] must be positive(km). Please check that.'}
    }
    else if (bus_stop == 2){
      if (L > 0.5 & L <= 0.6){tpk <- 157}
      else if (L > 0.6 & L <= 0.7){tpk <- 150}
      else if (L > 0.7 & L <= 0.8){tpk <- 145}
      else if (L > 0.8 & L <= 0.9){tpk <- 140}
      else if (L > 0.9){tpk <- 136}
      else {tpk <- 'Error : [L] must be > 0.5(km). Please check that.'}
    }
    else {tpk <- 'Error : [bus_stop] must be one of 1, 2. Please check that.'}
  }
  else if (ovrtk_lane == 'no'){
    if (bus_stop == 0){
      if (L > 0 & L <= 0.1){tpk <- 91}
      else if (L > 0.1 & L <= 0.2){tpk <- 78}
      else if (L > 0.2 & L <= 0.3){tpk <- 73}
      else if (L > 0.3 & L <= 0.4){tpk <- 69}
      else if (L > 0.4 & L <= 0.5){tpk <- 66}
      else if (L > 0.5 & L <= 0.6){tpk <- 64}
      else if (L > 0.6 & L <= 0.7){tpk <- 63}
      else if (L > 0.7 & L <= 0.8){tpk <- 61}
      else if (L > 0.8 & L <= 0.9){tpk <- 60}
      else if (L > 0.9){tpk <- 59}
      else {tpk <- 'Error : [L] must be positive(km). Please check that.'}
    }
    else if (bus_stop == 1){
      if (L > 0 & L <= 0.1){tpk <- 374}
      else if (L > 0.1 & L <= 0.2){tpk <- 223}
      else if (L > 0.2 & L <= 0.3){tpk <- 181}
      else if (L > 0.3 & L <= 0.4){tpk <- 159}
      else if (L > 0.4 & L <= 0.5){tpk <- 146}
      else if (L > 0.5 & L <= 0.6){tpk <- 136}
      else if (L > 0.6 & L <= 0.7){tpk <- 129}
      else if (L > 0.7 & L <= 0.8){tpk <- 124}
      else if (L > 0.8 & L <= 0.9){tpk <- 119}
      else if (L > 0.9){tpk <- 116}
      else {tpk <- 'Error : [L] must be positive(km). Please check that.'}
    }
    else if (bus_stop == 2){
      if (L > 0.5 & L <= 0.6){tpk <- 175}
      else if (L > 0.6 & L <= 0.7){tpk <- 168}
      else if (L > 0.7 & L <= 0.8){tpk <- 162}
      else if (L > 0.8 & L <= 0.9){tpk <- 158}
      else if (L > 0.9){tpk <- 154}
      else {tpk <- 'Error : [L] must be > 0.5(km). Please check that.'}
    }
    else {tpk <- 'Error : [bus_stop] must be one of 0, 1, 2. Please check that.'}
  }
  tpk
}
