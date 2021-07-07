#' Level of Service for Arterial Road with Exclusive Central Bus Lane
#'
#' Service level standards for arterial roads with exclusive central bus lanes
#'     The standard of arterial road 'type1' is applied.
#'     It follows <Table 12-12> in KHCM(2013), p.554.
#' @param ATS Average travel speed(kph)
#' @keywords level of service LOS arterial road ATS average travel speed
#' @export LOS_bus_artl Level of Service. \code{A}, \code{B}, \code{C}, \code{D}, \code{E}, \code{F}, \code{FF}, \code{FFF}
#' @seealso \code{\link{LOS_artl}}
#' @examples
#' LOS_bus_artl(ATS = 39.2)
#' LOS_bus_artl(ATS = 59)
LOS_bus_artl <- function(ATS = NULL){
  los <- LOS_artl(type = 'type1', ATS = ATS)
  los
}
