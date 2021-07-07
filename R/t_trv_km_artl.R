#' Travel Time Per km on Arterial Road
#'
#' Travel time per km on the arterial road(sec/km).
#'     It follows <Table 12-5> in KHCM(2013) p.536.
#' @param type Arterial road type. Choose one from : \code{'type1'}, \code{'type2'}, \code{'type3'}. See \code{\link{type_artl}}
#' @param friction Degree of roadside friction. Choose one from: \code{'big'}, \code{'small'}. See \code{\link{friction_artl}}
#' @param L Section length(km)
#' @keywords travel time arterial road per km
#' @seealso \code{\link{type_artl}}, \code{\link{friction_artl}}, \code{\link{ATS_artl}}
#' @details
#'     - If the analysis section of the arterial road is divided into several subsections,
#'     it is necessary to find the travel time per km after finding the average section length of the arterial road analysis section.
#'     - The traversing time thus found is multiplied by the total number of sections.
#'     - Alternatively, the traveling time is obtained for each sub-section.
#'     - Only the section length and roadside friction are considered as factors that affect cruising time.
#' @export t_trv_km_artl
#' @examples
#' t_trv_km_artl(type = 'type1', friction = 'big', L = 2.1)
t_trv_km_artl <- function(type = NULL, friction = NULL, L = NULL){
  if (type == 'type1'){
    if (friction == 'big'){
      if (L > 0 & L <= 0.1){tpk <- 108}
      else if (L > 0.1 & L <= 0.2){tpk <- 80}
      else if (L > 0.2 & L <= 0.3){tpk <- 71}
      else if (L > 0.3 & L <= 0.4){tpk <- 66}
      else if (L > 0.4 & L <= 0.5){tpk <- 63}
      else if (L > 0.5 & L <= 0.6){tpk <- 61}
      else if (L > 0.6 & L <= 0.7){tpk <- 60}
      else if (L > 0.7 & L <= 0.8){tpk <- 59}
      else if (L > 0.8 & L <= 0.9){tpk <- 58}
      else if (L > 0.9){tpk <- 58}
      else {tpk <- 'Error : [L] must be positive(km). Please check that.'}
    }
    else if (friction == 'small'){
      if (L > 0 & L <= 0.1){tpk <- 86}
      else if (L > 0.1 & L <= 0.2){tpk <- 66}
      else if (L > 0.2 & L <= 0.3){tpk <- 59}
      else if (L > 0.3 & L <= 0.4){tpk <- 56}
      else if (L > 0.4 & L <= 0.5){tpk <- 54}
      else if (L > 0.5 & L <= 0.6){tpk <- 53}
      else if (L > 0.6 & L <= 0.7){tpk <- 52}
      else if (L > 0.7 & L <= 0.8){tpk <- 51}
      else if (L > 0.8 & L <= 0.9){tpk <- 50}
      else if (L > 0.9){tpk <- 50}
      else {tpk <- 'Error : [L] must be positive(km). Please check that.'}
    }
    else {tpk <- 'Error : [friction] must be one of [big], [small]. Please check that.'}
  }
  else if (type == 'type2'){
    if (friction == 'big'){
      if (L > 0 & L <= 0.1){tpk <- 143}
      else if (L > 0.1 & L <= 0.2){tpk <- 100}
      else if (L > 0.2 & L <= 0.3){tpk <- 85}
      else if (L > 0.3 & L <= 0.4){tpk <- 77}
      else if (L > 0.4 & L <= 0.5){tpk <- 73}
      else if (L > 0.5 & L <= 0.6){tpk <- 70}
      else if (L > 0.6 & L <= 0.7){tpk <- 68}
      else if (L > 0.7 & L <= 0.8){tpk <- 66}
      else if (L > 0.8 & L <= 0.9){tpk <- 65}
      else if (L > 0.9){tpk <- 65}
      else {tpk <- 'Error : [L] must be positive(km). Please check that.'}
    }
    else if (friction == 'small'){
      if (L > 0 & L <= 0.1){tpk <- 102}
      else if (L > 0.1 & L <= 0.2){tpk <- 75}
      else if (L > 0.2 & L <= 0.3){tpk <- 67}
      else if (L > 0.3 & L <= 0.4){tpk <- 63}
      else if (L > 0.4 & L <= 0.5){tpk <- 60}
      else if (L > 0.5 & L <= 0.6){tpk <- 58}
      else if (L > 0.6 & L <= 0.7){tpk <- 57}
      else if (L > 0.7 & L <= 0.8){tpk <- 56}
      else if (L > 0.8 & L <= 0.9){tpk <- 55}
      else if (L > 0.9){tpk <- 54}
      else {tpk <- 'Error : [L] must be positive(km). Please check that.'}
    }
    else {tpk <- 'Error : [friction] must be one of [big], [small]. Please check that.'}
  }
  else if (type == 'type3'){
    if (friction == 'big'){
      if (L > 0 & L <= 0.1){tpk <- 178}
      else if (L > 0.1 & L <= 0.2){tpk <- 119}
      else if (L > 0.2 & L <= 0.3){tpk <- 99}
      else if (L > 0.3 & L <= 0.4){tpk <- 88}
      else if (L > 0.4 & L <= 0.5){tpk <- 83}
      else if (L > 0.5 & L <= 0.6){tpk <- 79}
      else if (L > 0.6 & L <= 0.7){tpk <- 75}
      else if (L > 0.7 & L <= 0.8){tpk <- 74}
      else if (L > 0.8 & L <= 0.9){tpk <- 72}
      else if (L > 0.9){tpk <- 72}
      else {tpk <- 'Error : [L] must be positive(km). Please check that.'}
    }
    else if (friction == 'small'){
      if (L > 0 & L <= 0.1){tpk <- 119}
      else if (L > 0.1 & L <= 0.2){tpk <- 85}
      else if (L > 0.2 & L <= 0.3){tpk <- 74}
      else if (L > 0.3 & L <= 0.4){tpk <- 69}
      else if (L > 0.4 & L <= 0.5){tpk <- 65}
      else if (L > 0.5 & L <= 0.6){tpk <- 63}
      else if (L > 0.6 & L <= 0.7){tpk <- 62}
      else if (L > 0.7 & L <= 0.8){tpk <- 61}
      else if (L > 0.8 & L <= 0.9){tpk <- 60}
      else if (L > 0.9){tpk <- 58}
      else {tpk <- 'Error : [L] must be positive(km). Please check that.'}
    }
    else {tpk <- 'Error : [friction] must be one of [big], [small]. Please check that.'}
  }
  else {tpk <- 'Error : [type] must be one of [type1], [type2], [type3]. Please check that.'}
  tpk
}
