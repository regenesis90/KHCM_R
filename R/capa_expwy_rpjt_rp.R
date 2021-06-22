#' Capacity of the Link of Ramp-Expressway Junction
#'
#' This function decides capacity of the ramp of ramp-expressway junction(pcph).
#'     It follows <Table 4-2> in KHCM(2013), p.85.
#' @param free_speed_ramp Free speed of ramp.(kph)
#' @param N_ramp Total number of ramp lanes(one-way). Choose one from : \code{1}, \code{2}
#' @keywords capacity expressway ramp junction
#' @details The capacity of the ramp is affected by the curve radius of the ramp,
#'     the difference in slope or slope from the main line, the shoulder width,
#'     the shape of the ramp, and the design speed of the ramp,
#'     which is a comprehensive concept of these.
#'     When it is necessary to determine the number of lanes of the ramp path itself,
#'     the ramp path capacity of this function can be used.
#' @export capa_expwy_rpjt_rp (pcph)
#' @examples
capa_expwy_rpjt_rp <- function(free_speed_ramp = NULL, N_ramp = NULL){
  if (N_ramp == 1){
    if (free_speed_ramp > 70){cap <- 2000}
    else if (free_speed_ramp > 60 & free_speed_ramp <= 70){cap <- 1900}
    else if (free_speed_ramp > 50 & free_speed_ramp <= 60){cap <- 1800}
    else if (free_speed_ramp > 40 & free_speed_ramp <= 50){cap <- 1700}
    else if (free_speed_ramp <= 40){cap <- 1600}
    else {'Error : [free_speed_ramp] must be positive(kph). Please check that.'}
  }
  else if (N_ramp == 2){
    if (free_speed_ramp > 70){cap <- 4000}
    else if (free_speed_ramp > 60 & free_speed_ramp <= 70){cap <- 3800}
    else if (free_speed_ramp > 50 & free_speed_ramp <= 60){cap <- 3600}
    else if (free_speed_ramp > 40 & free_speed_ramp <= 50){cap <- 3400}
    else if (free_speed_ramp <= 40){cap <- 3200}
    else {'Error : [free_speed_ramp] must be positive(kph). Please check that.'}
  }
  else {cap <- 'Error : [N_ramp] must be 1 or 2. Please check that.'}
  cap
}
