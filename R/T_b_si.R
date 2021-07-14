#' Saturation Headway Loss Time due to One Bus Stop at the Signalized Intersection
#'
#' Saturation headway loss time due to one bus stop at the signal intersection(sec).
#'     This function follows <Table 8-12> in KHCM(2013), p.231.
#' @param bus_stop_loc Bus stop location. Choose one from : \code{'driving_lane'}, \code{'separated'}
#' @param bus_pax Whether the bus has many or few passengers. Choose one from: \code{'small'}, \code{'middle'}, \code{'large'}
#' @keywords saturation headway loss time bus stop signalized intersection
#' @seealso \code{\link{L_bb_si}}
#' @details
#'     Details of Bus Stop location:
#'     - \code{\link{bus_pax = 'small'}} : Many bus users. Markets, department stores, bus terminals, transfer points by major train stations, etc.
#'     - \code{\link{bus_pax = 'middle'}} : Medium for bus users. General business districts, commercial districts, around subway stations, etc.
#'     - \code{\link{bus_pax = 'large'}}: Few bus users. general residential area, etc.
#' @export T_b_si Saturation head time increment according to one bus stop (T_b, sec)
#' @examples
#' T_b_si(bus_stop_loc = 'driving_lane', bus_pax = 'middle')
#' T_b_si(bus_stop_loc = 'separated')
T_b_si <- function(bus_stop_loc = NULL, bus_pax = NULL){
  if (bus_stop_loc == 'driving_lane'){
    if (bus_pax == 'small'){t <- 10.8}
    else if (bus_pax == 'middle'){t <- 15.3}
    else if (bus_pax == 'large'){t <- 22.8}
    else {t <- 'Error : [bus_pax] must be one of [small], [middle], [large]. Please check that.'}
  }
  else if (bus_stop_loc == 'separated'){t <- 1.4}
  else {t <- 'Error : [bus_stop_loc] must be one of [driving_lane], [separated]. Please check that.'}
  t
}
