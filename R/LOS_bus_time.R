#' Service level according to bus operation time(LOS_bus_time)
#'
#' It follows <Table 13-6> in KHCM(2013), p.595.
#' @param t operation time per day(hour)
#' @export LOS_bus_time \code{'A'}, \code{'B'}, \code{'C'}, \code{'D'}, \code{'E'}, \code{'F'}
#' @examples
#' LOS_bus_time(19)
LOS_bus_time <- function(t = NULL){
  if (t > 20 & t <= 24){los <- 'A'}
  if (t > 18 & t <= 20){los <- 'B'}
  if (t > 16 & t <= 18){los <- 'C'}
  if (t > 14 & t <= 16){los <- 'D'}
  if (t > 13 & t <= 14){los <- 'E'}
  if (t <= 13){los <- 'F'}
  los
}
