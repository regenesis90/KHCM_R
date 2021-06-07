#' Level of Service(LOS) by average travel speed on arterial roads(LOS_arterial)
#'
#' It follows <Table 12-1> in KHCM(2013) p.529
#' @param free_speed Choose one from: \code{80}, \code{70}, \code{60}
#' @param avg_speed *Numeric* Average travel speed(kph)
#' @keywords
#' @export LOS_arterial Level of Service. \code{A}, \code{B}, \code{C}, \code{D}, \code{E}, \code{F}, \code{FF}, \code{FFF}
#' @examples
#' LOS_arterial(free_speed = 70, avg_speed = 38)
#' LOS_arterial(80, 49)
LOS_arterial <- function(free_speed = NULL, avg_speed = NULL){
  if (free_speed == 80){
    if (avg_speed >= 67 & avg_speed <= 85){los <- 'A'}
    if (avg_speed >= 51 & avg_speed < 67){los <- 'B'}
    if (avg_speed >= 37 & avg_speed < 51){los <- 'C'}
    if (avg_speed >= 28 & avg_speed < 37){los <- 'D'}
    if (avg_speed >= 21 & avg_speed < 28){los <- 'E'}
    if (avg_speed >= 10 & avg_speed < 21){los <- 'F'}
    if (avg_speed >= 6 & avg_speed < 10){los <- 'FF'}
    if (avg_speed < 6){los <- 'FFF'}
  }
  if (free_speed == 70){
    if (avg_speed >= 60 & avg_speed <= 75){los <- 'A'}
    if (avg_speed >= 46 & avg_speed < 60){los <- 'B'}
    if (avg_speed >= 33 & avg_speed < 46){los <- 'C'}
    if (avg_speed >= 25 & avg_speed < 33){los <- 'D'}
    if (avg_speed >= 18 & avg_speed < 25){los <- 'E'}
    if (avg_speed >= 10 & avg_speed < 18){los <- 'F'}
    if (avg_speed >= 6 & avg_speed < 10){los <- 'FF'}
    if (avg_speed < 6){los <- 'FFF'}
  }
  if (free_speed == 60){
    if (avg_speed >= 49 & avg_speed <= 65){los <- 'A'}
    if (avg_speed >= 39 & avg_speed < 49){los <- 'B'}
    if (avg_speed >= 29 & avg_speed < 39){los <- 'C'}
    if (avg_speed >= 20 & avg_speed < 29){los <- 'D'}
    if (avg_speed >= 12 & avg_speed < 20){los <- 'E'}
    if (avg_speed >= 8 & avg_speed < 12){los <- 'F'}
    if (avg_speed >= 5 & avg_speed < 8){los <- 'FF'}
    if (avg_speed < 5){los <- 'FFF'}
  }
  los
}
