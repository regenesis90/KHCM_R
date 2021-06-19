#' Capacity of the Ramp of Ramp-Expressway Junction
#'
#' This function decides capacity of the ramp of ramp-expressway junction(pcph).
#'     It follows <Table 4-2> in KHCM(2013), p.85.
#' @param free_speed Free speed of ramp.(kph)
#' @param N_ramp Total number of ramp lanes(one-way). Choose one from : \code{1}, \code{2}
#' @keywords capacity expresswayy ramp junction
#' @details The capacity of the ramp is affected by the curve radius of the ramp,
#'     the difference in slope or slope from the main line, the shoulder width,
#'     the shape of the ramp, and the design speed of the ramp,
#'     which is a comprehensive concept of these.
#'     When it is necessary to determine the number of lanes of the ramp path itself,
#'     the ramp path capacity of this function can be used.
#' @export capa_expwy_rpjt_rp (pcph)
#' @examples
capa_expwy_rpjt_rp <- function(free_speed = NULL, N_ramp = NULL){
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
