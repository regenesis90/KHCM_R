#' Service level by region according to bus service interval(LOS_bus_interval)
#'
#' It follows <Table 13-5> in KHCM(2013), p.595.
#' @param city Choose one from: \code{'large'}, \code{'small'}
#' @param bus_interval bus operation interval(minutes)
#' @export LOS_bus_interval \code{'A'}, \code{'B'}, \code{'C'}, \code{'D'}, \code{'E'}, \code{'F'}
#' @examples
#' LOS_bus_interval('large', 23)
#' LOS_bus_interval('small', 92)
LOS_bus_interval <- function(city = NULL, bus_interval = NULL){
  if (city == 'large'){
    if (bus_interval > 0 & bus_interval <= 3){los <- 'A'}
    if (bus_interval > 3 & bus_interval <= 6){los <- 'B'}
    if (bus_interval > 6 & bus_interval <= 10){los <- 'C'}
    if (bus_interval > 10 & bus_interval <= 15){los <- 'D'}
    if (bus_interval > 15 & bus_interval <= 25){los <- 'E'}
    if (bus_interval > 25){los <- 'F'}
  }
  if (city == 'small'){
    if (bus_interval > 0 & bus_interval <= 10){los <- 'A'}
    if (bus_interval > 10 & bus_interval <= 20){los <- 'B'}
    if (bus_interval > 20 & bus_interval <= 40){los <- 'C'}
    if (bus_interval > 40 & bus_interval <= 60){los <- 'D'}
    if (bus_interval > 60 & bus_interval <= 100){los <- 'E'}
    if (bus_interval > 100){los <- 'F'}
  }
  los
}
