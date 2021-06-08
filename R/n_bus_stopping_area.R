#' Number of Bus Stop Stopping area(n_bus_stopping_area)
#'
#' It follows KHCM(2013), p.597
#' @param l Length of bus stop(m)
#' @param
#' @export n_bus_stopping_area
#' @examples
#' n_bus_stopping_area(12)
#' n_bus_stopping_area(11.999)
#' n_bus_stopping_area(25.142)
n_bus_stopping_area <- function(l = NULL){
  floor(l/12)
}
