#' Service level of cyclists on bicycle paths on city streets
#'
#' It follows <Table 15-7> in KHCM(2013), p.655
#' @param avg_speed Average speed(kph)
#' @keywords
#' @export LOS_bike_city_street
#' @examples
#' LOS_bike_city_street(avg_speed = 8.88)
LOS_bike_city_street <- function(avg_speed = NULL){
  if (avg_speed >= 0 & avg_speed <= 6){los <- 'F'}
  if (avg_speed > 6 & avg_speed <= 7){los <- 'E'}
  if (avg_speed > 7 & avg_speed <= 8){los <- 'D'}
  if (avg_speed > 8 & avg_speed <= 10){los <- 'C'}
  if (avg_speed > 10 & avg_speed <= 12){los <- 'B'}
  if (avg_speed > 12){los <- 'A'}
  los
}
