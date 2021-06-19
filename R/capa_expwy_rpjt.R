#' Capacity of the Ramp-Expressway Junction
#'
#' This function decides capacity of the ramp-expressway junction(pcph).
#'     It follows <Table 4-1> in KHCM(2013), p.84.
#' @param free_speed Free speed of main lane.(kph)
#' @param N Total number of main lanes(one-way). It must be 2 or more.
#' @param output Type of output capacity. Choose one from : \code{'main'}, \code{'influence_inflow'}, \code{'influence_outflow'}
#' @keywords capacity freeway ramp junction
#' @details The capacity of the highway junction.
#'     The capacity of the connecting path can be divided into the connecting path capacity and the main line capacity.
#'     The capacity of the junction is related to the maximum passing traffic at the point downstream of the upstream branching junction.
#'     This concept is not much different from the analysis concept of the basic highway section.
#'     - Classification section and junction main line traffic volume: At the junction, it means the capacity immediately after merging, and at the classification section just before classification.
#'     - Capacity of influenced area : Inflow traffic volume represents the junction area's impact area capacity, and outflow traffic volume represents the classification area's impact area capacity.
#' @export capa_expwy_rpjt
#' @examples
#' capa_expwy_rpjt(free_speed = 87, N = 3, output = 'main')
#' capa_expwy_rpjt(free_speed = 101, N = 4, output = 'influence_inflow')
capa_expwy_rpjt <- function(free_speed = NULL, N = NULL, output = NULL){
  if (output == 'main'){
    if (free_speed > 110 & free_speed <= 120){
      if (N == 2){cap <- 4600}
      else if (N == 3){cap <- 6900}
      else if (N >= 4){cap <- 2300 * N}
      else {cap <- 'Error : [N] must be >= 2. Please check that.'}
    }
    else if (free_speed > 100 & free_speed <= 110){
      if (N == 2){cap <- 4500}
      else if (N == 3){cap <- 6750}
      else if (N >= 4){cap <- 2250 * N}
      else {cap <- 'Error : [N] must be >= 2. Please check that.'}
    }
    else if (free_speed > 90 & free_speed <= 100){
      if (N == 2){cap <- 4400}
      else if (N == 3){cap <- 6600}
      else if (N >= 4){cap <- 2200 * N}
      else {cap <- 'Error : [N] must be >= 2. Please check that.'}
    }
    else if (free_speed > 0 & free_speed <= 90){
      if (N == 2){cap <- 4200}
      else if (N == 3){cap <- 6300}
      else if (N >= 4){cap <- 2100 * N}
      else {cap <- 'Error : [N] must be >= 2. Please check that.'}
    }
    else {cap <- 'Error : [free_speed] must be positive(kph). Please check that.'}
  }
  else if (output == 'influence_inflow'){
    if (free_speed > 110 & free_speed <= 120){cap <- 4400}
    else if (free_speed > 100 & free_speed <= 110){cap <- 4400}
    else if (free_speed > 90 & free_speed <= 100){cap <- 4400}
    else if (free_speed > 0 & free_speed <= 90){cap <- 4400}
    else {cap <- 'Error : [N] must be >= 2. Please check that.'}
  }
  else if (output == 'influence_outflow'){
    if (free_speed > 110 & free_speed <= 120){cap <- 4600}
    else if (free_speed > 100 & free_speed <= 110){cap <- 4600}
    else if (free_speed > 90 & free_speed <= 100){cap <- 4600}
    else if (free_speed > 0 & free_speed <= 90){cap <- 4600}
    else {cap <- 'Error : [N] must be >= 2. Please check that.'}
  }
  else {cap <- 'Error : [output] must be one of [main], [influence_inflow], [influence_outflow]. Please check that.'}
  cap
}
