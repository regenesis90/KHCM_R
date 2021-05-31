#' Saturation head time increment according to one bus stop (T_b, sec)
#'
#' This function follows <Table 8-12> in KHCM(2013)
#' @param bus_stop_loc *Categorical* Choose one from : \code{'driving_lane'}, \code{'separated'}
#' @param bus_passenger_size *Categorical* Choose one from: \code{'small'}, \code{'middle'}, \code{'large'}
#' @keywords
#' @export T_b Saturation head time increment according to one bus stop (T_b, sec)
#' @examples
#' T_b('driving_lane', 'middle')
#' T_b('separated')
T_b <- function(bus_stop_loc = NULL, bus_passenger_size = NULL){
  if (bus_stop_loc == 'driving_lane'){
    if (bus_passenger_size == 'small'){t <- 10.8}
    if (bus_passenger_size == 'middle'){t <- 15.3}
    if (bus_passenger_size == 'large'){t <- 22.8}
  }
  if (bus_stop_loc == 'separated'){t <- 1.4}
  t
}
