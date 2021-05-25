#' Level of Service(LOS) in Freeway Ramp Junction
#'
#' This function decides Level of Service(LOS) in the freeway ramp junction. It follows <Table 4-3> in KHCM(2013).
#' @param V *Numeric* The traffic volume of target road.
#' @param capacity *Numeric* The capacity of target road,
#' @param density *Numeric* The density of the road(pcpkmpl)
#' @export LOS_freeway_ramp_junction Average speed in freeway ramp junction.
#' @examples
#' LOS_freeway_ramp_junction(V = 3000, capacity = 4000, density = 23)
#' LOS_freeway_ramp_junction(4000, 3000, 24)
LOS_freeway_ramp_junction <- function(V = NULL, capacity = NULL, density = NULL){
  if (V >= 0 & capacity >= 0 & density >= 0){
    if (capacity >= V){
      if (density > 0 & density <= 6){LOS <-  'A'}
      if (density > 6 & density <= 12){LOS <- 'B'}
      if (density > 12 & density <= 17){LOS <- 'C'}
      if (density > 17 & density <= 22){LOS <- 'D'}
      if (density > 22){LOS <- 'E'}
    }
    else {
      LOS <- 'F'
    }
    LOS
  }
}
