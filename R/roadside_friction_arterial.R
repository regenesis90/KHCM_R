#' Degree of roadside friction on arterial roads(roadside_friction_arterial)
#'
#' It follows <Table 12-6> in KHCM(2013) p.536
#' @param free_speed Choose one from : \code{80}, \code{70}, \code{60}
#' @param bus_stop Number of bus stop(ea/km)
#' @param entrance_exit Number of entrance and exit(ea/km)
#' @keywords
#' @export roadside_friction_arterial
#' @examples
#' roadside_friction_arterial(80, 2, 3)
roadside_friction_arterial <- function(free_speed = NULL, bus_stop = NULL, entrance_exit = NULL){
  if (free_speed == 80){
    if (bus_stop > 2 | entrance_exit > 2){fr <- 'big'}
    if (bus_stop <= 2 | entrance_exit <= 2){fr <- 'small'}
  }
  if (free_speed == 70){
    if (bus_stop > 2 | entrance_exit > 3){fr <- 'big'}
    if (bus_stop <= 2 | entrance_exit <= 3){fr <- 'small'}
  }
  if (free_speed == 60){
    if (bus_stop > 2 | entrance_exit > 4){fr <- 'big'}
    if (bus_stop <= 2 | entrance_exit <= 4){fr <- 'small'}
  }
  fr
}
